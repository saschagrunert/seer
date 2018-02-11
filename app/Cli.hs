-- | This module contains everything related to command line argument parsing
--
-- @since 0.1.0

module Cli
  ( Args(..)
  , BasicCommand(..)
  , EntityCommand(..)
  , parser
  ) where

import Data.Monoid         ((<>))
import Options.Applicative (Parser, ParserInfo, command,
                            commandGroup, footer, fullDesc,
                            header, help, helper, info,
                            infoOption, long, progDesc,
                            subparser,  (<**>))
import Seer                (version)

-- | Representation of the main arguments passed to the application
--
-- @since 0.1.0
newtype Args = Args BasicCommand
  deriving (Show)

-- | Representation of all basic commands
--
-- @since 0.1.0
data BasicCommand
  = Create EntityCommand
  | Delete EntityCommand
  | Describe EntityCommand
  | Get EntityCommand
  deriving (Show)

-- | Representation of all basic commands
--
-- @since 0.1.0
data EntityCommand
  = Config
  | Storage
  | Action
  | Resource
  | Schedule
  deriving (Show)

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
  <> command "create" (info createParser (progDesc "Create an entity"))
  <> command "delete" (info deleteParser (progDesc "Delete an entity"))
  <> command
       "describe"
       (info describeParser (progDesc "Show details of a specific entity"))
  <> command "get" (info getParser (progDesc "Display one or many entities"))
  )

-- | Parse an entity command
--
-- @since 0.1.0
entityParser :: (EntityCommand -> a) -> Parser a
entityParser a = a <$> entityCommand <**> helper

-- | Parse the 'create' command
--
-- @since 0.1.0
createParser :: Parser BasicCommand
createParser = entityParser Create

-- | Parse the 'delete' command
--
-- @since 0.1.0
deleteParser :: Parser BasicCommand
deleteParser = entityParser Delete

-- | Parse the 'describe' command
--
-- @since 0.1.0
describeParser :: Parser BasicCommand
describeParser = entityParser Describe

-- | Parse the 'get' command
--
-- @since 0.1.0
getParser :: Parser BasicCommand
getParser = entityParser Get

-- | Parse the 'Entity' subcommand
--
-- @since 0.1.0
entityCommand :: Parser EntityCommand
entityCommand = subparser
  (  commandGroup "Available entities:"
  <> command "config"  (info configParser (progDesc "Select the 'Config'"))
  <> command "storage" (info storageParser (progDesc "Select the 'Storage'"))
  <> command "action"  (info actionParser (progDesc "Select the 'Action'"))
  <> command "resource"
             (info resourceParser (progDesc "Select the 'Resource'"))
  <> command "schedule"
             (info scheduleParser (progDesc "Select the 'Schedule'"))
  )

-- | Parse the 'Config' entity
--
-- @since 0.1.0
configParser :: Parser EntityCommand
configParser = pure Config

-- | Parse the 'Storage' entity
--
-- @since 0.1.0
storageParser :: Parser EntityCommand
storageParser = pure Storage

-- | Parse the 'Action' entity
--
-- @since 0.1.0
actionParser :: Parser EntityCommand
actionParser = pure Action

-- | Parse the 'Resource' entity
--
-- @since 0.1.0
resourceParser :: Parser EntityCommand
resourceParser = pure Resource

-- | Parse the 'Schedule' entity
--
-- @since 0.1.0
scheduleParser :: Parser EntityCommand
scheduleParser = pure Schedule
