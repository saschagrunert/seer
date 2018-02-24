-- | The main application module
--
-- @since 0.1.0

module Main
  ( main
  ) where

import Cli                 (Args (Args)
                           ,BasicCommand (Config
                                         ,Create
                                         ,Get)
                           ,ConfigCommand (GetConfig
                                          ,SetStorage)
                           ,CreateCommand (Action
                                          ,Resource
                                          ,Schedule
                                          ,Storage)
                           ,GetCommand (Actions
                                       ,Resources
                                       ,Schedules
                                       ,Storages)
                           ,parser)
import Options.Applicative (ParserPrefs (ParserPrefs
                                        ,prefBacktrack
                                        ,prefColumns
                                        ,prefDisambiguate
                                        ,prefMultiSuffix
                                        ,prefShowHelpOnEmpty
                                        ,prefShowHelpOnError)
                           ,customExecParser)
import Seer                (createAction
                           ,createResource
                           ,createSchedule
                           ,createStorage
                           ,getActions
                           ,getConfig
                           ,getResources
                           ,getSchedules
                           ,getStorages
                           ,setDefaultStorage)
import System.Console.ANSI (ConsoleLayer (Foreground)
                           ,Color (Red)
                           ,ColorIntensity (Vivid)
                           ,SGR (Reset
                                ,SetColor)
                           ,setSGR)
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
run (Args (Config (SetStorage n))) = call $ setDefaultStorage n
run (Args (Config GetConfig     )) = call getConfig
run (Args (Create (Action n r d))) = call $ createAction n d r
run (Args (Create (Resource n d m t w h f s u))) =
  call $ createResource n d (m, t, w, h, f, s, u)
run (Args (Create (Schedule f r a))) = call $ createSchedule f r a
run (Args (Create (Storage n r   ))) = call $ createStorage n r
run (Args (Get    Actions         )) = call getActions
run (Args (Get    Resources       )) = call getResources
run (Args (Get    Schedules       )) = call getSchedules
run (Args (Get    Storages        )) = call getStorages

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
    >> putStrLn ("✗ " ++ e)
    >> setSGR [Reset]
    >> exitFailure
