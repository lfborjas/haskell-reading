{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module HCat where

import qualified System.Environment as Env
import qualified Control.Exception as Exception
import qualified System.IO.Error as IOError
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.ByteString as BS
import qualified System.Info
import qualified System.Process as Process
import System.IO
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeFormat
import qualified Data.Time.Clock.POSIX as PosixClock
import qualified System.Directory as Directory
import qualified Text.Printf as Printf
import Data.Bool (bool)

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int
  , screenColumns :: Int
  } deriving Show

data ContinueCancel
  = Continue
  | Cancel
  deriving (Eq, Show)

data FileInfo = FileInfo
  { filePath :: FilePath
  , fileSize :: Int
  , fileMTime :: Clock.UTCTime
  , fileReadable :: Bool
  , fileWriteable :: Bool
  , fileExecutable :: Bool
  } deriving Show

runHCat :: IO ()
runHCat =
  -- NOTE(luis) the book no longer handles exceptions at this point
  Exception.handle handleErr $ do
    targetFilePath <- do
      args <- handleArgs
      eitherToErr args
    contents <- do
      handle' <- openFile targetFilePath ReadMode 
      TextIO.hGetContents handle'
    
    termSize <- getTerminalSize

    hSetBuffering stdout NoBuffering
    
    finfo <- fileInfo targetFilePath
    let pages = paginate termSize finfo contents
    showPages pages
  where
    handleErr :: IOError -> IO ()
    handleErr e = putStrLn "Error!" >> print @IOError e

-- using throwIO vs. throw since it would otherwise be
-- "unpredictable" when running in IO
eitherToErr :: Show a => Either a b -> IO b
eitherToErr (Right a) = pure a
eitherToErr (Left e)  = Exception.throwIO . IOError.userError $ show e

handleArgs :: IO (Either String FilePath)
handleArgs =
  parseArgs <$> Env.getArgs
  where
    parseArgs argumentList =
      case argumentList of
        [fname] -> Right fname
        []      -> Left "No filename provided"
        _       -> Left "Multiple files not supported"

getContinue :: IO ContinueCancel
getContinue =
  -- disable line buffering: otherwise we wouldn't get input until a newline is introduced
  hSetBuffering stdin NoBuffering
  -- disable echoing so user input is not echoed back in the terminal
  >> hSetEcho stdin False
  >> hGetChar stdin
  >>= \case
    ' ' -> return Continue
    'q' -> return Cancel
    _   -> getContinue

showPages :: [Text.Text] -> IO ()
showPages [] = return ()
showPages (page:pages) =
  clearScreen
  >> TextIO.putStrLn page
  >> getContinue
  >>= \case
      Continue -> showPages pages
      Cancel   -> return ()

-- TODO: look up this crazy escape seq
clearScreen :: IO ()
clearScreen = BS.putStr "\^[[1J\^[[1;1H"

fileInfo :: FilePath -> IO FileInfo
fileInfo filePath = do
  perms <- Directory.getPermissions filePath
  mtime <- Directory.getModificationTime filePath
  -- NOTE(luis) the _code_ says Text, but the explanation
  -- (rightly) says bytestring:
  size <- BS.length <$> BS.readFile filePath
  return FileInfo
    { filePath = filePath
    , fileSize = size
    , fileMTime = mtime
    , fileReadable = Directory.readable perms
    , fileWriteable = Directory.writable perms
    , fileExecutable = Directory.executable perms
    }

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text.Text
formatFileInfo FileInfo{..} maxWidth totalPages currentPage =
  invertTextColors (truncateStatus statusLine)
  where
  statusLine = Text.pack $
    Printf.printf
    "%s | permissions: %s | %d bytes | modified: %s | page %d of %d"
    filePath
    permissionString
    fileSize
    timestamp
    currentPage
    totalPages
  permissionString =
    [ bool 'r' '-' fileReadable
    , bool 'w' '-' fileWriteable
    , bool 'x' '-' fileExecutable
    ]
  timestamp =
    TimeFormat.formatTime TimeFormat.defaultTimeLocale  "%F %T" fileMTime
  truncateStatus sl
    | maxWidth <= 3 = ""
    | Text.length sl > maxWidth = Text.take (maxWidth - 3) statusLine <> "..."
    | otherwise = sl
  invertTextColors inputStr =
    let
      reverseVideo = "\^[[7m"
      resetVideo  = "\^[[0m"
    in reverseVideo <> inputStr <> resetVideo


-- >>> groupsOf 3 [1,2,3,4,5,6,7]
-- [[1,2,3],[4,5,6],[7]]
-- >>> groupsOf 0 [1,2,3]
-- []
groupsOf :: Int -> [a] -> [[a]]
-- handles the edge case that the book left as an exercise for the reader
groupsOf n _ | n <= 0 = []
groupsOf n [] | n > 0  = []
groupsOf n elems =
  let (hd, tl) = splitAt n elems
  in hd : groupsOf n tl

-- | Attempt to softwrap at space boundaries before splitting at the max width
-- >>> wordWrap 6 "word wrapping is tricky"
-- ["word","wrappi","ng is","tricky"]
wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap lineLength lineText
  | Text.length lineText <= lineLength = [lineText]
  | otherwise =
    let
      (candidate, nextLines) = Text.splitAt lineLength lineText
      (firstLine, overflow)  = softWrap candidate (Text.length candidate - 1)
    in firstLine : wordWrap lineLength (overflow <> nextLines)
  where
    softWrap hardWrappedText textIndex
      | textIndex <= 0 = (hardWrappedText, Text.empty)
      | Text.index hardWrappedText textIndex == ' ' =
        let (wrappedLine, rest) = Text.splitAt textIndex hardWrappedText
        in (wrappedLine, Text.tail rest)
      | otherwise = softWrap hardWrappedText (textIndex - 1)

paginate :: ScreenDimensions -> FileInfo -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) finfo text =
  let unwrappedLines = Text.lines text
      rows' = rows - 1
      wrappedLines   = concatMap (wordWrap cols) unwrappedLines
      pages =
        map (Text.unlines . padTo rows') $ groupsOf rows' wrappedLines
      pageCount = length pages
      statusLines = map (formatFileInfo finfo cols pageCount) [1..pageCount]
  in zipWith (<>) pages statusLines
  where
    padTo :: Int -> [Text.Text] -> [Text.Text]
    padTo lineCount rowsToPad =
      take lineCount $ rowsToPad <> repeat ""

-- | System-dependent dimension deviser -- uses `tput`
getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  case System.Info.os of
    "darwin" -> tputScreenDimensions
    "linux"  -> tputScreenDimensions
    _other   -> pure $ ScreenDimensions 25 80
  where
    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions =
      Process.readProcess "tput" ["lines"] ""
      >>= \linien ->
        Process.readProcess "tput" ["cols"] ""
        >>= \cols ->
          let lines' = read . init $ linien
              cols'  = read . init $ cols
          in return $ ScreenDimensions lines' cols'
