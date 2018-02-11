-- | The main application module
--
-- @since 0.1.0

module Main
  ( main
  ) where

import Cli                 (Args (Args), BasicCommand (Get),
                            EntityCommand (Action, Config,
                            Resource, Schedule, Storage), parser)
import Options.Applicative (ParserPrefs (ParserPrefs, prefBacktrack,
                            prefColumns, prefDisambiguate,
                            prefMultiSuffix, prefShowHelpOnEmpty,
                            prefShowHelpOnError), customExecParser)
import Seer                (getActions, getConfig, getResources,
                            getSchedules, getStorages)
import System.Console.ANSI (ConsoleLayer (Foreground), Color (Red),
                            ColorIntensity (Vivid),
                            SGR (Reset, SetColor), setSGR)
import System.Exit         (exitFailure)

-- | The application entry point
--
-- @since 0.1.0
main :: IO ()
main = customExecParser p parser >>= run
 where
  p = ParserPrefs
    { prefMultiSuffix     = ""
    , prefDisambiguate    = True
    , prefShowHelpOnError = True
    , prefShowHelpOnEmpty = True
    , prefBacktrack       = True
    , prefColumns         = 80
    }

-- | The application path based on the input arguments
--
-- @since 0.1.0
run :: Args -> IO ()
run (Args (Get Config  )) = call getConfig
run (Args (Get Storage )) = call getStorages
run (Args (Get Action  )) = call getActions
run (Args (Get Schedule)) = call getSchedules
run (Args (Get Resource)) = call getResources
run _                     = exitError "Unkown argument"

-- | Call a function and print the result.
--
-- @since 0.1.0
call :: IO (Either String String) -> IO ()
call f = f >>= either exitError putStrLn

-- | Prints an error and exits with `1`.
--
-- @since 0.1.0
exitError :: String -> IO ()
exitError e =
  setSGR [SetColor Foreground Vivid Red]
    >> putStrLn ("Error: " ++ e)
    >> setSGR [Reset]
    >> exitFailure
