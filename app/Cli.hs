-- | This module contains everything related to command line argument parsing
--
-- @since 0.1.0

module Cli
  ( Args(..)
  , BasicCommand(..)
  , ConfigCommand(..)
  , CreateCommand(..)
  , DeleteCommand(..)
  , EditCommand(..)
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
                           ,optional
                           ,progDesc
                           ,short
                           ,strArgument
                           ,strOption
                           ,subparser
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
data BasicCommand = Config ConfigCommand
                  | Get GetCommand
                  | Create CreateCommand
                  | Delete DeleteCommand
                  | Edit EditCommand
                  | View { viewFrom :: String
                         , viewTo :: String
                         , viewResource :: Maybe String
                         , viewAction :: Maybe String }

-- | Representation of all possible 'get' commands
--
-- @since 0.1.0
data ConfigCommand = GetConfig
                   | SetStorage String

-- | Representation of all possible 'get' commands
--
-- @since 0.1.0
data GetCommand = GetStorages
                | GetActions
                | GetResources
                | GetSchedules

-- | Representation of all 'create' commands
--
-- @since 0.1.0
data CreateCommand = CreateStorage { createStorageName :: String          -- ^ Name of the Storage
                                   , createStorageRemote :: Maybe String  -- ^ Remote of the Storage
                                   }
                   | CreateAction { createActionName :: String              -- ^ Name of the Action
                                  , createActionDuration :: String          -- ^ Duration of the Action
                                  , createActionDescription :: Maybe String -- ^ Description of the Action
                                  }
                   | CreateResource { createResourceName :: String              -- ^ Name of the Resource
                                    , createResourceDescription :: Maybe String -- ^ Description of the Resource
                                    , createResourceMon :: Maybe String         -- ^ Mon of the Resource
                                    , createResourceTue :: Maybe String         -- ^ Tue of the Resource
                                    , createResourceWed :: Maybe String         -- ^ Wed of the Resource
                                    , createResourceThu :: Maybe String         -- ^ Thu of the Resource
                                    , createResourceFri :: Maybe String         -- ^ Fri of the Resource
                                    , createResourceSat :: Maybe String         -- ^ Sat of the Resource
                                    , createResourceSun :: Maybe String         -- ^ Sun of the Resource
                                    }
                   | CreateSchedule { createScheduleStart :: String     -- ^ Start of the Schedule
                                    , createScheduleResource :: String -- ^ Resource of the Schedule
                                    , createScheduleAction :: String   -- ^ Action of the Schedule
                                    }

-- | Representation of all possible 'delete' commands
--
-- @since 0.1.0
data DeleteCommand = DeleteStorage String   -- ^ Name of the Storage
                   | DeleteAction String    -- ^ Name of the Action
                   | DeleteResource String  -- ^ Name of the Resource
                   | DeleteSchedule String  -- ^ Number of the Schedule

-- | Representation of all possible 'edit' commands
--
-- @since 0.1.0
data EditCommand = EditStorage { editStorageName :: String            -- ^ The actual name of the Storage
                               , editStorageNewName :: Maybe String   -- ^ The new Name of the Storage
                               , editStorageNewRemote :: Maybe String -- ^ The new Remote of the Storage
                               }
                 | EditAction { editActionName :: String                  -- ^ The actual name of the Action
                              , editActionNewName :: Maybe String         -- ^ The new name of the Action
                              , editActionNewDescription :: Maybe String  -- ^ The new description of the Action
                              , editActionNewDuration :: Maybe String     -- ^ The new duration of the Action
                              }
                 | EditResource { editResourceName :: String                 -- ^ The Name of the Resource
                                , editResourceNewName :: Maybe String        -- ^ The new Name of the Resource
                                , editResourceNewDescription :: Maybe String -- ^ The new Description of the Resource
                                , editResourceNewMon :: Maybe String         -- ^ The new Mon of the Resource
                                , editResourceNewTue :: Maybe String         -- ^ The new Tue of the Resource
                                , editResourceNewWed :: Maybe String         -- ^ The new Wed of the Resource
                                , editResourceNewThu :: Maybe String         -- ^ The new Thu of the Resource
                                , editResourceNewFri :: Maybe String         -- ^ The new Fri of the Resource
                                , editResourceNewSat :: Maybe String         -- ^ The new Sat of the Resource
                                , editResourceNewSun :: Maybe String         -- ^ The new Sun of the Resource
                                }
                 | EditSchedule { editScheduleNumber :: String            -- ^ The number of the Schedule
                                , editScheduleNewResource :: Maybe String -- ^ The new Resource of the Schedule
                                , editScheduleNewAction :: Maybe String   -- ^ The new Action of the Schedule
                                , editScheduleNewStart :: Maybe String    -- ^ The new start of the Schedule
                                }

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
  <> command "get"    (info getParser (progDesc "Display one or many entities"))
  <> command "create" (info createParser (progDesc "Create an entity"))
  <> command "delete" (info deleteParser (progDesc "Delete an entity"))
  <> command "edit"   (info editParser (progDesc "Edit an entity"))
  <> command "view"   (info viewParser (progDesc "View the Agenda"))
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
                ( info (pure GetStorages <$> helper)
                       (progDesc "Show all available Storages")
                )
           <> command
                "actions"
                ( info (pure GetActions <$> helper)
                       (progDesc "Show all Actions for the default Storage")
                )
           <> command
                "resources"
                ( info
                  (pure GetResources <$> helper)
                  (progDesc "Show all Resources for the default Storage")
                )
           <> command
                "schedules"
                ( info (pure GetSchedules <$> helper)
                       (progDesc "Show Schedules for the default Storage")
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
                  <*>  optional
                         ( strOption
                           (  long "remote"
                           <> short 'r'
                           <> metavar "REMOTE"
                           <> help "Optional remote location of a storage"
                           )
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
                  <*>  optional
                         ( strOption
                           (  long "description"
                           <> short 'd'
                           <> metavar "DESCRIPTION"
                           <> help "Optional description for the Action"
                           )
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
                  <*>  optional
                         ( strOption
                           (  long "description"
                           <> short 'd'
                           <> metavar "DESCRIPTION"
                           <> help "Optional description for the Resource"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "mon"
                           <> short 'm'
                           <> metavar "HH:MM-HH:MM"
                           <> help "Monday availability"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "tue"
                           <> short 't'
                           <> metavar "HH:MM-HH:MM"
                           <> help "Tuesday availability"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "wed"
                           <> short 'w'
                           <> metavar "HH:MM-HH:MM"
                           <> help "Wednesday availability"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "thu"
                           <> short 'h'
                           <> metavar "HH:MM-HH:MM"
                           <> help "Thursday availability"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "fri"
                           <> short 'f'
                           <> metavar "HH:MM-HH:MM"
                           <> help "Friday availability"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "sat"
                           <> short 's'
                           <> metavar "HH:MM-HH:MM"
                           <> help "Saturday availability"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "sun"
                           <> short 'u'
                           <> metavar "HH:MM-HH:MM"
                           <> help "Sunday availability"
                           )
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
                  (progDesc "Create a new Schedule within the default Storage")
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
                  (    DeleteSchedule
                  <$>  strArgument (metavar "NUMBER")
                  <**> helper
                  )
                  (progDesc "Delete a Schedule by its number")
                )
           )
    <**> helper

-- | Parse the 'edit' command
--
-- @since 0.1.0
editParser :: Parser BasicCommand
editParser =
  Edit
    <$>  subparser
           (  commandGroup "Edit entities:"
           <> command
                "storage"
                ( info
                  (    EditStorage
                  <$>  strArgument (metavar "NAME")
                  <*>  optional
                         ( strOption
                           (  long "name"
                           <> short 'n'
                           <> metavar "NEW_NAME"
                           <> help "Change the name of a storage"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "remote"
                           <> short 'r'
                           <> metavar "NEW_REMOTE"
                           <> help
                                "Change the remote location of a storage. \
                                \For remote removal pass an '-' argument"
                           )
                         )
                  <**> helper
                  )
                  (progDesc "Edit a Storage by its name")
                )
           <> command
                "action"
                ( info
                  (    EditAction
                  <$>  strArgument (metavar "NAME")
                  <*>  optional
                         ( strOption
                           (  long "name"
                           <> short 'n'
                           <> metavar "NEW_NAME"
                           <> help "Change the name of the Action"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "description"
                           <> short 'd'
                           <> metavar "NEW_DESCRIPTION"
                           <> help "Change the description of the Action"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "duration"
                           <> short 'r'
                           <> metavar "NEW_DURATION"
                           <> help "Change the duration of the Action"
                           )
                         )
                  <**> helper
                  )
                  (progDesc "Edit an Action by its name")
                )
           <> command
                "resource"
                ( info
                  (    EditResource
                  <$>  strArgument (metavar "NAME")
                  <*>  optional
                         ( strOption
                           (  long "name"
                           <> short 'n'
                           <> metavar "NEW_NAME"
                           <> help "Change the name of the Resource"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "description"
                           <> short 'd'
                           <> metavar "NEW_DESCRIPTION"
                           <> help "The new description for the Resource"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "mon"
                           <> short 'm'
                           <> metavar "HH:MM-HH:MM"
                           <> help "The new Monday availability"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "tue"
                           <> short 't'
                           <> metavar "HH:MM-HH:MM"
                           <> help "The new Tuesday availability"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "wed"
                           <> short 'w'
                           <> metavar "HH:MM-HH:MM"
                           <> help "The new Wednesday availability"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "thu"
                           <> short 'h'
                           <> metavar "HH:MM-HH:MM"
                           <> help "The new Thursday availability"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "fri"
                           <> short 'f'
                           <> metavar "HH:MM-HH:MM"
                           <> help "The new Friday availability"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "sat"
                           <> short 's'
                           <> metavar "HH:MM-HH:MM"
                           <> help "The new Saturday availability"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "sun"
                           <> short 'u'
                           <> metavar "HH:MM-HH:MM"
                           <> help "The new Sunday availability"
                           )
                         )
                  <**> helper
                  )
                  (progDesc "Edit a Resource by its name")
                )
           <> command
                "schedule"
                ( info
                  (    EditSchedule
                  <$>  strArgument (metavar "NUMBER")
                  <*>  optional
                         ( strOption
                           (  long "start"
                           <> short 's'
                           <> metavar "NEW_START"
                           <> help "Change the start date of a Schedule"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "resource"
                           <> short 'r'
                           <> metavar "NEW_RESOURCE"
                           <> help "Change the Resource of a Schedule"
                           )
                         )
                  <*>  optional
                         ( strOption
                           (  long "action"
                           <> short 'a'
                           <> metavar "NEW_ACTION"
                           <> help "Change the Action of a Schedule"
                           )
                         )
                  <**> helper
                  )
                  (progDesc "Edit a Schedule by its number")
                )
           )
    <**> helper

-- | Parse the 'view' command
--
-- @since 0.1.0
viewParser :: Parser BasicCommand
viewParser =
  View
    <$>  strArgument
           ( metavar "FROM" <> value "now" <> help
             "The starting date, defaults to the current date"
           )
    <*>  strArgument
           ( metavar "TO" <> value "in 1 week" <> help
             "The ending date, defaults to the end of the next week"
           )
    <*>  optional
           ( strOption
             ( long "resource" <> short 'r' <> metavar "RESOURCE" <> help
               "Optional resource selection"
             )
           )
    <*>  optional
           ( strOption
             ( long "action" <> short 'a' <> metavar "ACTION" <> help
               "Optional action selection"
             )
           )
    <**> helper
