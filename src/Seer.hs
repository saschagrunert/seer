-- | This module everything related to the main library interface
--
-- @since 0.1.0

module Seer
  ( MonadSeer
  , getActions
  , getConfig
  , getResources
  , getSchedules
  , getStorages
  , setDefaultStorage
  , version
  ) where

import           Control.Monad          ((>=>))
import           Data.Bifunctor         (bimap, second)
import           Data.List              (transpose)
import qualified Seer.Action as A       (Action)
import           Seer.Availability      (WeekDay(..))
import qualified Seer.Config as C       (Config, new, storage)
import           Seer.Manifest          (Manifest, spec, ToList (toList))
import qualified Seer.Resource as R     (Resource)
import qualified Seer.Schedule as S     (Schedule)
import           Seer.Storage           (Storage, list, loadConfig,
                                         loadActions, loadResources,
                                         loadSchedules, saveConfig,
                                         storageExist)
import           System.IO.Error        (ioeGetErrorString)
import           Text.PrettyPrint.Boxes (hsep, left, render, text, vcat)

-- | The version of the library
--
-- @since 0.1.0
version :: String
version = "0.1.0"

-- | A abstraction Monad to isolate real IO Actions
--
-- @since 0.1.0
class Monad m => MonadSeer m where
  list' :: m (Either IOError [[String]])
  loadActions' ::  Storage -> m (Either IOError [A.Action])
  loadConfig' :: m (Either IOError C.Config)
  loadResources' ::  Storage -> m (Either IOError [R.Resource])
  loadSchedules' ::  Storage -> m (Either IOError [S.Schedule])
  newConfig' :: String -> m C.Config
  saveConfig' :: C.Config -> m (Either IOError ())
  storageExist' :: Storage -> m (Either IOError Bool)

-- | The implementation of the isolation abstraction for the IO Monad
--
-- @since 0.1.0
instance MonadSeer IO where
  list' = list
  loadActions' = loadActions
  loadConfig' = loadConfig
  loadResources' = loadResources
  loadSchedules' = loadSchedules
  newConfig' = C.new
  saveConfig' = saveConfig
  storageExist' = storageExist

-- | A reference to an Error Message Storage
--
-- @since 0.1.0
type Error = String

-- | Convert an IOError to an 'Error' on the Left side of an 'Either' type
--
-- @since 0.1.0
eitherToErrorOr
  :: (MonadSeer m)
  => (a -> m (Either Error b))
  -> Either IOError a
  -> m (Either Error b)
eitherToErrorOr = either (return . Left . ioeGetErrorString)

-- | Function combinator for error conversion purposes
--
-- @since 0.1.0
(>>|)
  :: MonadSeer m
  => m (Either IOError a)
  -> (a -> m (Either Error b))
  -> m (Either Error b)
l >>| r = l >>= eitherToErrorOr r

-- | Maps the 'Left' part of an 'Either' from an 'IOError' to an 'Error'
--
-- @since 0.1.0
bimapIOError :: (a -> b) -> Either IOError a -> Either Error b
bimapIOError = bimap ioeGetErrorString

-- | Creates a string table from a two dimensional String list
--
-- @since 0.1.0
tablyfy
  :: [[String]] -- ^ The list list to be formatted
  -> String     -- ^ The result
tablyfy r = render . hsep 2 left $ vcat left . map text <$> transpose r

-- | The standard message when nothing was found
--
-- @since 0.1.0
nf :: String
nf = "✗ Nothing found"

-- | The standard message when an operation succeded.
--
-- @since 0.1.0
done :: String
done = "✓ Done"

-- | The standard table header for a name
--
-- @since 0.1.0
name :: String
name = "NAME"

-- | The standard table header for a description
--
-- @since 0.1.0
description :: String
description = "DESCRIPTION"

-- | The standard table header for a duration
--
-- @since 0.1.0
duration :: String
duration = "DURATION"

-- | The standard table header for start dates
--
-- @since 0.1.0
from :: String
from = "FROM"

-- | The standard table header for end dates
--
-- @since 0.1.0
to :: String
to = "TO"

-- | The standard table header for a resource
--
-- @since 0.1.0
resource :: String
resource = "RESOURCE"

-- | The standard table header for an action
--
-- @since 0.1.0
action :: String
action = "ACTION"

-- | The standard table header for a remote
--
-- @since 0.1.0
remote :: String
remote = "REMOTE"

-- | The standard table header for the default storage
--
-- @since 0.1.0
defaultStorage :: String
defaultStorage = "DEFAULT STORAGE"

-- | Set a default Storage for a given name. Fails if the Storage does not
-- exist or an IO error occurs
--
-- @since 0.1.0
setDefaultStorage
  :: MonadSeer m
  => Storage                 -- ^ The name of the Storage to be set
  -> m (Either Error String) -- ^ The result
setDefaultStorage n = storageExist' n >>| f
 where
  f e = if e
    then bimapIOError (const done) <$> (newConfig' n >>= saveConfig')
    else return $ Left "✗ Storage does not exist"

-- | Get a default Storage. Fails if an IO error occurs.
--
-- @since 0.1.0
getDefaultStorage :: MonadSeer m => m (Either IOError String)
getDefaultStorage = second (C.storage . spec) <$> loadConfig'

-- | List all available Storages if possible. Evaluates to either a fully
-- formatted table (Right) or an error message (Left).
--
-- @since 0.1.0
getStorages :: MonadSeer m => m (Either Error String)
getStorages =
  bimapIOError (\r -> if null r then nf else tablyfy ([name, remote] : r))
    <$> list'

-- | List the current Configuration if possible. Evaluates to either a fully
-- formatted table (Right) or an error message (Left).
--
-- @since 0.1.0
getConfig :: MonadSeer m => m (Either Error String)
getConfig =
  loadConfig'
    >>| (\r -> return . Right $ tablyfy [pure defaultStorage, toList r])

-- | Function for receiving an entity and formatting the output as String table
--
-- @since 0.1.0
getAndTabilifyEntity
  :: (MonadSeer m, ToList a)
  => (String -> m (Either IOError [Manifest a])) -- ^ The entity retrieval function
  -> [String]                                    -- ^ The table headers
  -> m (Either Error String)
getAndTabilifyEntity f h =
  getDefaultStorage
    >>| ( f >=> eitherToErrorOr
          ( \r -> return . Right $ if null r
            then nf
            else tablyfy $ h : (toList <$> r)
          )
        )

-- | List all available Actions for the default Storage. Evaluates to either a
-- fully formatted table (Right) or an error message (Left).
--
-- @since 0.1.0
getActions :: MonadSeer m => m (Either Error String)
getActions = getAndTabilifyEntity loadActions' [name, description, duration]

-- | List all available Resources for the default Storage. Evaluates to either
-- a fully formatted table (Right) or an error message (Left).
--
-- @since 0.1.0
getResources :: MonadSeer m => m (Either Error String)
getResources = getAndTabilifyEntity
  loadResources'
  ([name, description] ++ (show <$> [Mon .. Sun]))

-- | List all available Schedules for the default Storage. Evaluates to either
-- a fully formatted table (Right) or an error message (Left).
--
-- @since 0.1.0
getSchedules :: MonadSeer m => m (Either Error String)
getSchedules = getAndTabilifyEntity loadSchedules' [from, to, resource, action]
