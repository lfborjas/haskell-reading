{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module HCat where

import qualified System.Environment as Env
import qualified Control.Exception as Exception
import qualified System.IO.Error as IOError

runHCat :: IO ()
runHCat =
  -- NOTE(luis) a @withErrorHandling@ is used
  -- in the book vs. the infix version of 'catch'
  (handleArgs 
  >>= eitherToErr
  >>= readFile
  >>= putStrLn) `Exception.catch` handleErr
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
