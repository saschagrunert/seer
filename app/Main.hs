-- | The main application module
--
-- @since 0.1.0
module Main where

import Cli                 (Args (Args), BasicCommand (Get),
                            EntityCommand (Storage), parser)
import Options.Applicative (ParserPrefs (ParserPrefs, prefBacktrack, prefColumns, prefDisambiguate, prefMultiSuffix, prefShowHelpOnEmpty, prefShowHelpOnError),
                            customExecParser)
import Seer                (getStorages)
import System.Console.ANSI

-- | The application entry point
--
-- @since 0.1.0
main :: IO ()
main = customExecParser p parser >>= run
  where
    p =
      ParserPrefs
      { prefMultiSuffix = ""
      , prefDisambiguate = True
      , prefShowHelpOnError = True
      , prefShowHelpOnEmpty = True
      , prefBacktrack = True
      , prefColumns = 80
      }

-- | The application path based on the input arguments
--
-- @since 0.1.0
run :: Args -> IO ()
run (Args _ (Get Storage)) = do
  l <- getStorages
  case l of
    Right s -> putStr s
    Left e -> do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Could not list Storages: \n" ++ e
      setSGR [Reset]
run _ = print "Unknown pattern"
