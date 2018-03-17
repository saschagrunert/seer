-- | This module includes everything related to Storage handling.
--
-- @since 0.1.0

module Seer.Storage
  ( MonadStorage
  , Storage
  , configExist
  , editName
  , editRemote
  , list
  , loadActions
  , loadConfig
  , loadResources
  , loadSchedules
  , new
  , remove
  , removeActions
  , removeResources
  , removeSchedules
  , save
  , saveActions
  , saveConfig
  , saveResources
  , saveSchedules
  , storageExist
  ) where

import Control.Lens          ((^.))
import Control.Exception     (try)
import Control.Monad         (foldM
                             ,mapM
                             ,(>=>))
import Data.Bifunctor        (first)
import Data.List             (sort
                             ,zipWith)
import Data.Maybe            (maybe)
import Data.UUID             (toString)
import Data.Yaml             (FromJSON
                             ,ParseException
                             ,ToJSON
                             ,decodeFileEither
                             ,encodeFile
                             ,prettyPrintParseException)
import Seer.Action           (Action)
import Seer.Config           (Config)
import Seer.Git              (runGitCommand
                             ,runGitCommandIO)
import Seer.Manifest         (Manifest
                             ,metadata
                             ,uid)
import Seer.Resource         (Resource)
import Seer.Schedule         (Schedule)
import Seer.Utils            (rstrip
                             ,(>>-))
import System.Directory      (createDirectory
                             ,createDirectoryIfMissing
                             ,doesDirectoryExist
                             ,doesFileExist
                             ,getHomeDirectory
                             ,listDirectory
                             ,removeDirectoryRecursive
                             ,renameDirectory
                             ,removeFile)
import System.FilePath.Glob  (compile
                             ,globDir1)
import System.FilePath.Posix ((<.>)
                             ,(</>))
import System.IO.Error       (IOError
                             ,userError)
import Text.Printf           (printf)

-- | A reference to a Storage
--
-- @since 0.1.0
type Storage = String

-- | A abstraction Monad to isolate real IO Actions
--
-- @since 0.1.0
class Monad m => MonadStorage m where
  decodeFileEither' :: FromJSON a => FilePath -> m (Either ParseException a)
  doesFileExist' :: FilePath -> m Bool
  runGitCommand' :: String -> String -> m (Either String String)
  runGitCommandIO' :: String -> String -> m (Either IOError ())
  tryCreateDirectory' :: FilePath -> m (Either IOError ())
  tryCreateDirectoryIfMissing' :: FilePath -> m (Either IOError ())
  tryEncodeFile' :: ToJSON a => FilePath -> a -> m (Either IOError ())
  tryGetHomeDirectory' :: m (Either IOError FilePath)
  tryListDirectory' :: FilePath -> m (Either IOError [FilePath])
  tryListYamlFiles' :: FilePath -> m (Either IOError [FilePath])
  tryRemoveDirectoryRecursive' :: FilePath -> m (Either IOError ())
  tryRemoveFile' :: FilePath -> m (Either IOError ())
  tryRenameDirectory' :: FilePath -> FilePath -> m (Either IOError ())
  tryWriteFile' :: FilePath -> String -> m (Either IOError ())

-- | The implementation of the isolation abstraction for the IO Monad
--
-- @since 0.1.0
instance MonadStorage IO where
    decodeFileEither' = decodeFileEither
    doesFileExist' = doesFileExist
    runGitCommand' = runGitCommand
    runGitCommandIO' = runGitCommandIO
    tryCreateDirectory' = try . createDirectory
    tryCreateDirectoryIfMissing' f = try $ createDirectoryIfMissing True f
    tryEncodeFile' p e = try $ encodeFile p e
    tryGetHomeDirectory' = try getHomeDirectory
    tryListDirectory' f = try $ sort <$> listDirectory f
    tryListYamlFiles' f = doesDirectoryExist f
      >>= (\l ->
        if l
        then try . flip globDir1 f . compile $ "*." ++ yamlExt
        else rlu $ "Directory does not exist: " ++ f)
    tryRemoveDirectoryRecursive' = try . removeDirectoryRecursive
    tryRemoveFile' = try . removeFile
    tryRenameDirectory' a b = try $ renameDirectory a b
    tryWriteFile' f s = try $ writeFile f s

-- | A helper function for functor FilePath appending
--
-- @since 0.1.0
(<//>)
  :: (Functor f2, Functor f1)
  => f1 (f2 FilePath) -- ^ The input FilePath
  -> FilePath         -- ^ The FilePath to be appended
  -> f1 (f2 FilePath) -- ^ The resulting FilePath
a <//> b = fmap (</> b) <$> a

-- | A helper function for a Left userError
--
-- @since 0.1.0
rlu :: MonadStorage m => String -> m (Either IOError a)
rlu = return . Left . userError

-- | The global Seer directory
--
-- @since 0.1.0
seerDir :: MonadStorage m => m (Either IOError FilePath)
seerDir = tryGetHomeDirectory' <//> ".seer"

-- | The global storage cache directory
--
-- @since 0.1.0
cacheDir :: MonadStorage m => m (Either IOError FilePath)
cacheDir = seerDir <//> "cache"

-- |  The storage directory
--
-- @since 0.1.0
storageDir
  :: MonadStorage m
  => Storage                     -- ^ The name of the storage
  -> m (Either IOError FilePath) -- ^ The result
storageDir f = cacheDir <//> f

-- | The storage internal 'Actions' directory
--
-- @since 0.1.0
actionsDir
  :: MonadStorage m
  => Storage                     -- ^ The name of the storage
  -> m (Either IOError FilePath) -- ^ The result
actionsDir n = storageDir n <//> "actions"

-- | The storage internal global 'Resources' directory
--
-- @since 0.1.0
resourcesDir
  :: MonadStorage m
  => Storage                     -- ^ The name of the storage
  -> m (Either IOError FilePath) -- ^ The result
resourcesDir n = storageDir n <//> "resources"

-- | The storage internal global 'Schedules' directory
--
-- @since 0.1.0
schedulesDir
  :: MonadStorage m
  => Storage                     -- ^ The name of the storage
  -> m (Either IOError FilePath) -- ^ The result
schedulesDir n = storageDir n <//> "schedules"

-- | The file where the configuration is stored
--
-- @since 0.1.0
configPath :: MonadStorage m => m (Either IOError FilePath)
configPath = seerDir <//> ("config" <.> yamlExt)

-- | The standard .keep file for empty git repository folders
--
-- @since 0.1.0
keepFile :: FilePath
keepFile = ".keep"

-- | The standard YAML file extension
--
-- @since 0.1.0
yamlExt :: FilePath
yamlExt = "yaml"

-- | Helper for monadic 'Either' function application
--
-- @since 0.1.0
(>>>)
  :: Monad m
  => m (Either a b) -- ^ The monadic value to be unwrapped
  -> m (Either a c) -- ^ The value to be 'return'
  -> m (Either a c) -- ^ The result
a >>> f = a >>- return f

-- | Run a git command in the storage dir of the given storage name.
--
-- @since 0.1.0
runGit
  :: (MonadStorage m)
  => [String]              -- ^ The git commands
  -> Storage               -- ^ The name of the storage
  -> m (Either IOError ()) -- ^ The result
runGit c n = foldM (const go) (Right ()) c
  where go x = storageDir n >>- runGitCommandIO' x

-- | Create all needed Storage directories for a given name
--
-- @since 0.1.0
createStorageDirs
  :: MonadStorage m
  => Storage               -- ^ The name of the storage
  -> m (Either IOError ()) -- ^ The result
createStorageDirs n = foldM
  (\_ f -> f n >>- tryCreateDirectory')
  (Right ())
  [storageDir, actionsDir, resourcesDir, schedulesDir]

-- | Create .keep files in every storage related directory
--
-- @since 0.1.0
createKeepFiles
  :: MonadStorage m
  => Storage               -- ^ The name of the storage
  -> m (Either IOError ()) -- ^ The result
createKeepFiles n = foldM
  (\_ f -> f n >>- (\p -> tryWriteFile' (p </> keepFile) ""))
  (Right ())
  [actionsDir, resourcesDir, schedulesDir]

-- | Create the global cache directory for all storages
--
-- @since 0.1.0
createCacheDir :: (MonadStorage m) => m (Either IOError ())
createCacheDir = cacheDir >>- tryCreateDirectoryIfMissing'

-- | Lists all currently available storage instances including their remote
-- location
--
-- @since 0.1.0
list :: (MonadStorage m) => m (Either IOError [[String]])
list = listStorages >>- (\d -> (Right . zipWith f d) <$> mapM r d)
 where
  f a b = [a, b]
  r e = fr "" <$> remote e "get-url origin"
  fr _ (Right b) = b
  fr b _         = b

-- | Evaluates to `True` if a `Storage` exists
--
-- @since 0.1.0
storageExist
  :: MonadStorage m
  => Storage                 -- ^ The name of the Storage
  -> m (Either IOError Bool) -- ^ The result
storageExist n = listStorages >>- (\s -> return . Right $ n `elem` s)

-- | Lists all currently available storage instances
--
-- @since 0.1.0
listStorages :: (MonadStorage m) => m (Either IOError [FilePath])
listStorages = cacheDir >>- tryListDirectory'

-- | Returns a remote for a given Storage
--
-- @since 0.1.0
remote
  :: MonadStorage m
  => Storage                   -- ^ The name of the storage
  -> String                    -- ^ An addition to the remote fetch command
  -> m (Either IOError String) -- ^ The result
remote n e =
  storageDir n
    >>- (   runGitCommand' ("remote " ++ e)
        >=> either rlu (return . Right . rstrip)
        )

-- | Create a new storage for the given name and optional remote location. This
-- means in general it will:
--
-- 1. Create the cache directory on ~/.seer
-- 2. Create all needed initial data
-- 3. Setup the git repository locally
-- 4. Push the initial commit to the remote location if necessary.
--
-- If anything went wrong an IOError will be in the result. The cleanup will
-- take care that no artifacts will be left on the device.
--
-- Examples:
--
-- >>> Seer.Storage.new "newStorage" Nothing
-- Right ()
--
-- >>> Seer.Storage.remove "newStorage"
-- Right ()
--
-- @since 0.1.0
new
  :: MonadStorage m
  => Storage               -- ^ The name of the storage
  -> Maybe String          -- ^ The remote URL of the storage
  -> m (Either IOError ()) -- ^ The result
new n r =
  createCacheDir
    >>> createStorageDirs n
    >>> (   createKeepFiles n
        >>> g ["init", "add .", "commit -m Init"]
        >>> maybe
              (return $ Right ())
              ( \x ->
                g [printf "remote add origin '%s'" x, "push -u origin master"]
              )
              r
        )
    >>= h
 where
  g c = runGit c n
  h (Left e) = remove n >>- (\_ -> return $ Left e)
  h e        = return e

-- | Save the current storage and sync with the remote location if necessary.
--
-- @since 0.1.0
save
  :: MonadStorage m
  => Storage               -- ^ The name of the storage
  -> m (Either IOError ()) -- ^ The result
save n = runGit ["add .", "commit --allow-empty -m Update"] n >>> sync n

-- | Sync the storage with the remote location if necessary
--
-- @since 0.1.0
sync
  :: MonadStorage m
  => Storage               -- ^ The name of the storage
  -> m (Either IOError ()) -- ^ The result
sync d =
  storageDir d
    >>- ( runGitCommand' "remote" >=> either
          rlu
          ( \o ->
            if null o then return (Right ()) else runGit ["pull", "push"] d
          )
        )

-- | Remove a storage for the given name
--
-- @since 0.1.0
remove
  :: MonadStorage m
  => Storage                 -- ^ The name of the storage
  -> m (Either IOError ())   -- ^ The result
remove n = storageDir n >>- tryRemoveDirectoryRecursive'

-- | Load from the given 'FilePath' and return either a error or a valid
-- instance of 'FromJSON a'
--
-- @since 0.1.0
loadFile
  :: (FromJSON a, MonadStorage m)
  => FilePath             -- ^ The location of the file to be parsed
  -> m (Either IOError a) -- ^ The result
loadFile a =
  first (userError . prettyPrintParseException) <$> decodeFileEither' a

-- | Load all files for the given name of the storage
--
-- @since 0.1.0
loadFiles
  :: (FromJSON a, MonadStorage m)
  => [FilePath]             -- ^ The list of FilePaths
  -> m (Either IOError [a]) -- ^ The result
loadFiles =
  foldM (\a v -> loadFile v >>= (\r -> return ((:) <$> r <*> a))) $ Right []

-- | Load certain entities from a directory into a list
--
-- @since 0.1.0
loadEntities
  :: (FromJSON a, MonadStorage m)
  => m (Either IOError FilePath)  -- ^ The input filepath
  -> m (Either IOError [a])       -- ^ The output list
loadEntities n = n >>- tryListYamlFiles' >>- loadFiles

-- | Load all 'Action's for the given name of the storage.
--
-- @since 0.1.0
loadActions
  :: MonadStorage m
  => Storage                     -- ^ The name of the storage
  -> m (Either IOError [Action]) -- ^ The result
loadActions = loadEntities . actionsDir

-- | Load all Resources for the given name of the storage.
--
-- @since 0.1.0
loadResources
  :: MonadStorage m
  => Storage                       -- ^ The name of the storage
  -> m (Either IOError [Resource]) -- ^ The result
loadResources = loadEntities . resourcesDir

-- | Load all Schedules for the given name of the storage.
--
-- @since 0.1.0
loadSchedules
  :: MonadStorage m
  => Storage                       -- ^ The name of the storage
  -> m (Either IOError [Schedule]) -- ^ The result
loadSchedules = loadEntities . schedulesDir

-- | Load the Config from the configuration directory.
--
-- @since 0.1.0
loadConfig :: MonadStorage m => m (Either IOError Config)
loadConfig = configPath >>- loadFile

-- | Check if the configuration actually exist
--
-- @since 0.1.0
configExist :: MonadStorage m => m (Either IOError Bool)
configExist = configPath >>- (doesFileExist' >=> return . Right)

-- | Save the given list of 'ToJSON a' to the 'FilePath'
--
-- @since 0.1.0
saveFiles
  :: (ToJSON a, MonadStorage m)
  => [Manifest a]          -- ^ The instances to be stored
  -> FilePath              -- ^ The location of the saved files
  -> m (Either IOError ()) -- ^ The result
saveFiles a p =
  foldM (\_ v -> tryEncodeFile' (p </> entityFilename v) v) (Right ()) a

-- | Evaluates to a general filename convention for entities
--
-- @since 0.1.0
entityFilename :: Manifest s -> FilePath
entityFilename v = toString (v ^. metadata . uid) <.> yamlExt

-- | Save certain entities from a list into a directory
--
-- @since 0.1.0
saveEntities
  :: (MonadStorage m, ToJSON a)
  => (t -> m (Either IOError FilePath))   -- ^ The target directory
  -> t                                    -- ^ The name of the storage
  -> [Manifest a]                         -- ^ The entities to be stored
  -> m (Either IOError ())                -- ^ The result
saveEntities f n a = f n >>- saveFiles a

-- | Save all 'Action's for the given name of the storage.
--
-- @since 0.1.0
saveActions
  :: MonadStorage m
  => Storage               -- ^ The name of the storage
  -> [Action]              -- ^ The 'Action's to be stored
  -> m (Either IOError ()) -- ^ The result
saveActions = saveEntities actionsDir

-- | Save all 'Resource's for the given name of the storage.
--
-- @since 0.1.0
saveResources
  :: MonadStorage m
  => Storage               -- ^ The name of the storage
  -> [Resource]            -- ^ The 'Resource's to be stored
  -> m (Either IOError ()) -- ^ The result
saveResources = saveEntities resourcesDir

-- | Save all 'Schedule's for the given name of the storage.
--
-- @since 0.1.0
saveSchedules
  :: MonadStorage m
  => Storage               -- ^ The name of the storage
  -> [Schedule]            -- ^ The 'Schedule's to be stored
  -> m (Either IOError ()) -- ^ The result
saveSchedules = saveEntities schedulesDir

-- | Saved the given config within the storage. Also creates needed directories
-- if missing.
--
-- @since 0.1.0
saveConfig
  :: MonadStorage m
  => Config                -- ^ The configuration to be saved
  -> m (Either IOError ()) -- ^ The result
saveConfig c =
  seerDir
    >>- tryCreateDirectoryIfMissing'
    >>> configPath
    >>- (`tryEncodeFile'` c)

-- | A generic entity removal helper
--
-- @since 0.1.0
removeEntity
  :: MonadStorage m
  => (t -> m (Either IOError FilePath)) -- ^ The directory path
  -> t                                  -- ^ The name of the Storage
  -> Manifest s                         -- ^ The entity to be removed
  -> m (Either IOError ())              -- ^ The result
removeEntity f n u = f n <//> entityFilename u >>- tryRemoveFile'

-- | A generic entity list removal helper
--
-- @since 0.1.0
removeEntities
  :: (MonadStorage m, Foldable t1)
  => (t2 -> m (Either IOError FilePath))
  -> t2
  -> t1 (Manifest s)
  -> m (Either IOError ())
removeEntities f n = foldM (const $ removeEntity f n) (Right ())

-- | Remove a list of 'Action's for a given Storage
--
-- @since 0.1.0
removeActions
  :: MonadStorage m
  => Storage               -- ^ The name of the storage
  -> [Action]              -- ^ The 'Action's to be removed
  -> m (Either IOError ()) -- ^ The result
removeActions = removeEntities actionsDir

-- | Remove a list of 'Resource's for a given Storage
--
-- @since 0.1.0
removeResources
  :: MonadStorage m
  => Storage               -- ^ The name of the storage
  -> [Resource]            -- ^ The 'Resource's to be removed
  -> m (Either IOError ()) -- ^ The result
removeResources = removeEntities resourcesDir

-- | Remove a list of 'Schedule's for a given Storage
--
-- @since 0.1.0
removeSchedules
  :: MonadStorage m
  => Storage               -- ^ The name of the storage
  -> [Schedule]            -- ^ The 'Schedule's to be removed
  -> m (Either IOError ()) -- ^ The result
removeSchedules = removeEntities schedulesDir

-- | Change the name of a given Storage. Errors on any IO Error and if the new
-- Storage already exist
--
-- @since 0.1.0
editName
  :: MonadStorage m
  => Storage               -- ^ The name of the old Storage
  -> Storage               -- ^ The name of the new Storage
  -> m (Either IOError ()) -- ^ The result
editName o n | n == o    = rlu "Old and new Storage names do not differ"
             | null n    = rlu "Impossible new storage name"
             | otherwise = storageExist o >>- (\x -> storageExist n >>- c x)
 where
  c False _    = rlu "Storage does not exist"
  c _     True = rlu "New storage already exist"
  c _     _    = storageDir o >>- (\x -> storageDir n >>- tryRenameDirectory' x)

-- | Change the remote location of a given Storage.
--
-- @since 0.1.0
editRemote
  :: MonadStorage m
  => Storage               -- ^ The name of the Storage
  -> String                -- ^ The new remote location
  -> m (Either IOError ()) -- ^ The result
editRemote n r = remote n "" >>- c
 where
  c "" | null r    = return $ Right ()
       | otherwise = go "add"
  c _ | null r    = go "remove"
      | otherwise = go "set-url"
  go x = runGit [printf "remote %s origin '%s'" x r] n
