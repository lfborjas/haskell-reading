{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module HCat where

import qualified System.Environment as Env
import qualified Control.Exception as Exception
import qualified System.IO.Error as IOError
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.ByteString as BS

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int
  , screenColumns :: Int
  } deriving Show

runHCat :: IO ()
runHCat =
  -- NOTE(luis) a @withErrorHandling@ is used
  -- in the book vs. the infix version of 'catch'
  (handleArgs 
  >>= eitherToErr
  >>= TextIO.readFile
  >>= TextIO.putStrLn) `Exception.catch` handleErr
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

paginate :: ScreenDimensions -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) text =
  let unwrappedLines = Text.lines text
      wrappedLines   = concatMap (wordWrap cols) unwrappedLines
      pageLines      = groupsOf rows wrappedLines
  in map Text.unlines pageLines
