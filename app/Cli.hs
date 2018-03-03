-- | This module contains everything related to command line argument parsing
--
-- @since 0.1.0

module Cli
  ( Args(..)
  , BasicCommand(..)
  , ConfigCommand(..)
  , CreateCommand(..)
  , DeleteCommand(..)
  , GetCommand(..)
  , parser
  ) where

import Data.Monoid         ((<>))
import Options.Applicative (Parser
                           ,ParserInfo
                           ,command
                           ,commandGroup
                           ,footer
                           ,fullDesc
                           ,header
                           ,help
                           ,helper
                           ,info
                           ,infoOption
                           ,long
                           ,metavar
                           ,progDesc
                           ,short
                           ,strArgument
                           ,strOption
                           ,subparser
                           ,switch
                           ,value
                           ,(<**>))
import Seer                (version)

-- | Representation of the main arguments passed to the application
--
-- @since 0.1.0
newtype Args = Args BasicCommand

-- | Representation of all basic commands
--
-- @since 0.1.0
data BasicCommand
  = Config ConfigCommand
  | Create CreateCommand
  | Delete DeleteCommand
  | Get GetCommand

-- | Representation of all possible 'get' commands
--
-- @since 0.1.0
data ConfigCommand
  = GetConfig
  | SetStorage String

-- | Representation of all 'create' commands
--
-- @since 0.1.0
data CreateCommand = CreateStorage String  -- ^ Name
                                   String  -- ^ Remote
                   | CreateAction String   -- ^ Name
                                  String   -- ^ Description
                                  String   -- ^ Duration
                   | CreateResource String -- ^ Name
                                    String -- ^ Description
                                    String -- ^ Mon
                                    String -- ^ Tue
                                    String -- ^ Wed
                                    String -- ^ Thu
                                    String -- ^ Fri
                                    String -- ^ Sat
                                    String -- ^ Sun
                   | CreateSchedule String -- ^ Start
                                    String -- ^ Resource
                                    String -- ^ Action

-- | Representation of all possible 'delete' commands
--
-- @since 0.1.0
data DeleteCommand
  = DeleteStorage String
  | DeleteAction String
  | DeleteResource String
  | DeleteSchedule String

-- | Representation of all possible 'get' commands
--
-- @since 0.1.0
data GetCommand
  = Storages
  | Actions
  | Resources
  | Schedules Bool -- ^ Specifies if all schedules should be shown


-- | Parse the main arguments
--
-- @since 0.1.0
parser :: ParserInfo Args
parser = info
  (args <**> versionParser <**> helper)
  ( fullDesc <> header "seer - A collaborative resource planning tool" <> footer
    "More info at <https://github.com/saschagrunert/seer>"
  )

-- | The main argument parser
--
-- @since 0.1.0
args :: Parser Args
args = Args <$> basicCommand

-- | Returns the version of the application
--
-- @since 0.1.0
versionParser :: Parser (a -> a)
versionParser =
  infoOption version (long "version" <> help "Print the current version")

-- | Parse the basic commands
--
-- @since 0.1.0
basicCommand :: Parser BasicCommand
basicCommand = subparser
  (  commandGroup "Basic commands:"
  <> command "config"
             (info configParser (progDesc "Configure the environment"))
  <> command "create" (info createParser (progDesc "Create an entity"))
  <> command "delete" (info deleteParser (progDesc "Delete an entity"))
  <> command "get"    (info getParser (progDesc "Display one or many entities"))
  )

-- | Parse the 'config' command
--
-- @since 0.1.0
configParser :: Parser BasicCommand
configParser =
  Config
    <$>  subparser
           (  commandGroup "Configurable entities:"
           <> command
                "get"
                ( info (pure GetConfig <$> helper)
                       (progDesc "Show the default used storage")
                )
           <> command
                "set-storage"
                ( info
                  (SetStorage <$> strArgument (metavar "NAME") <**> helper)
                  (progDesc "Set the default storage to be used")
                )
           )
    <**> helper

-- | Parse the 'get' command
--
-- @since 0.1.0
getParser :: Parser BasicCommand
getParser =
  Get
    <$>  subparser
           (  commandGroup "Displayable entities:"
           <> command
                "storages"
                ( info (pure Storages <$> helper)
                       (progDesc "Show all available Storages")
                )
           <> command
                "actions"
                ( info (pure Actions <$> helper)
                       (progDesc "Show all Actions for the default Storage")
                )
           <> command
                "resources"
                ( info
                  (pure Resources <$> helper)
                  (progDesc "Show all Resources for the default Storage")
                )
           <> command
                "schedules"
                ( info
                  (    Schedules
                  <$>  switch
                         (  long "all"
                         <> short 'a'
                         <> help
                              "Show all Schedules instead of the only future ones"
                         )
                  <**> helper
                  )
                  (progDesc "Show all Schedules for the default Storage")
                )
           )
    <**> helper

-- | Parse the 'create' command
--
-- @since 0.1.0
createParser :: Parser BasicCommand
createParser =
  Create
    <$>  subparser
           (  commandGroup "Createable entities:"
           <> command
                "storage"
                ( info
                  (    CreateStorage
                  <$>  strArgument (metavar "NAME")
                  <*>  strOption
                         (  long "remote"
                         <> short 'r'
                         <> metavar "REMOTE"
                         <> value ""
                         <> help "Optional remote location of a storage"
                         )
                  <**> helper
                  )
                  (progDesc "Create a new Storage and set it as new default")
                )
           <> command
                "action"
                ( info
                  (    CreateAction
                  <$>  strArgument (metavar "NAME")
                  <*>  strArgument
                         (  metavar "DURATION"
                         <> help
                              "The time the action will take, \
                              \like '1y', '2w', '3d', '4h', '5m'"
                         )
                  <*>  strOption
                         (  long "description"
                         <> short 'd'
                         <> metavar "DESCRIPTION"
                         <> value ""
                         <> help "Optional description for the Action"
                         )
                  <**> helper
                  )
                  (progDesc "Create a new Action within the default Storage")
                )
           <> command
                "resource"
                ( info
                  (    CreateResource
                  <$>  strArgument (metavar "NAME")
                  <*>  strOption
                         (  long "description"
                         <> short 'd'
                         <> metavar "DESCRIPTION"
                         <> value ""
                         <> help "Optional description for the Resource"
                         )
                  <*>  strOption
                         (  long "mon"
                         <> short 'm'
                         <> metavar "HH:MM-HH:MM"
                         <> value ""
                         <> help "Monday availability"
                         )
                  <*>  strOption
                         (  long "tue"
                         <> short 't'
                         <> metavar "HH:MM-HH:MM"
                         <> value ""
                         <> help "Tuesday availability"
                         )
                  <*>  strOption
                         (  long "wed"
                         <> short 'w'
                         <> metavar "HH:MM-HH:MM"
                         <> value ""
                         <> help "Wednesday availability"
                         )
                  <*>  strOption
                         (  long "thu"
                         <> short 'h'
                         <> metavar "HH:MM-HH:MM"
                         <> value ""
                         <> help "Thursday availability"
                         )
                  <*>  strOption
                         (  long "fri"
                         <> short 'f'
                         <> metavar "HH:MM-HH:MM"
                         <> value ""
                         <> help "Friday availability"
                         )
                  <*>  strOption
                         (  long "sat"
                         <> short 's'
                         <> metavar "HH:MM-HH:MM"
                         <> value ""
                         <> help "Saturday availability"
                         )
                  <*>  strOption
                         (  long "sun"
                         <> short 'u'
                         <> metavar "HH:MM-HH:MM"
                         <> value ""
                         <> help "Sunday availability"
                         )
                  <**> helper
                  )
                  (progDesc "Create a new Resource within the default Storage")
                )
           <> command
                "schedule"
                ( info
                  (    CreateSchedule
                  <$>  strArgument
                         ( metavar "START" <> help
                           "The date and time when the Schedule starts"
                         )
                  <*>  strArgument
                         ( metavar "RESOURCE" <> help
                           "The name of the Resource which should be used"
                         )
                  <*>  strArgument
                         ( metavar "ACTION" <> help
                           "The name of the Action which should be done"
                         )
                  <**> helper
                  )
                  (progDesc "Create a new Storage and set it as new default")
                )
           )
    <**> helper

-- | Parse the 'delete' command
--
-- @since 0.1.0
deleteParser :: Parser BasicCommand
deleteParser =
  Delete
    <$>  subparser
           (  commandGroup "Delete entities:"
           <> command
                "storage"
                ( info
                  (DeleteStorage <$> strArgument (metavar "NAME") <**> helper)
                  (progDesc "Delete a Storage by its name")
                )
           <> command
                "action"
                ( info
                  (DeleteAction <$> strArgument (metavar "NAME") <**> helper)
                  (progDesc "Delete a Action by its name")
                )
           <> command
                "resource"
                ( info
                  (DeleteResource <$> strArgument (metavar "NAME") <**> helper)
                  (progDesc "Delete a Resource by its name")
                )
           <> command
                "schedule"
                ( info
                  (DeleteSchedule <$> strArgument (metavar "DATE") <**> helper)
                  (progDesc "Delete a Schedule by its start date")
                )
           )
    <**> helper
