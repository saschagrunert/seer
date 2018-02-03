-- | This module includes everything related to Storage handling.
--
-- @since 0.1.0

module Seer.Storage (
    MonadStorage,
    loadActions,
    loadResources,
    loadSchedules,
    new,
    remove,
    saveActions,
    saveResources,
    saveSchedules,
) where

import Control.Exception (try)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Maybe (maybe)
import Data.Yaml (FromJSON
                 ,ParseException
                 ,ToJSON
                 ,decodeFileEither
                 ,encodeFile
                 ,prettyPrintParseException)
import Seer.Git (MonadGit, runGitCommandIO)
import Seer.Action (Action)
import Seer.Manifest (IsManifest(uuidString))
import Seer.Resource (Resource)
import Seer.Schedule (Schedule)
import System.Directory (createDirectory
                        ,createDirectoryIfMissing
                        ,getHomeDirectory
                        ,removeDirectoryRecursive)
import System.FilePath.Glob (compile, globDir1)
import System.FilePath.Posix ((</>), (<.>))
import System.IO.Error (IOError, userError)


-- | The global storage cache directory
--
-- @since 0.1.0
cacheDir :: MonadStorage m => m (Either IOError FilePath)
cacheDir = fmap (</> ".seer") <$> tryGetHomeDirectory'

-- |  The storage directory
--
-- @since 0.1.0
storageDir
    :: MonadStorage m
    => String                      -- ^ The name of the storage
    -> m (Either IOError FilePath) -- ^ The result
storageDir f = fmap (</> f) <$> cacheDir

-- | The storage internal 'Actions' directory
--
-- @since 0.1.0
actionsDir
    :: MonadStorage m
    => String                      -- ^ The name of the storage
    -> m (Either IOError FilePath) -- ^ The result
actionsDir n = fmap (</> "actions") <$> storageDir n

-- | The storage internal global 'Resources' directory
--
-- @since 0.1.0
resourcesDir
    :: MonadStorage m
    => String                      -- ^ The name of the storage
    -> m (Either IOError FilePath) -- ^ The result
resourcesDir n = fmap (</> "resources") <$> storageDir n

-- | The storage internal global 'Schedules' directory
--
-- @since 0.1.0
schedulesDir
    :: MonadStorage m
    => String                      -- ^ The name of the storage
    -> m (Either IOError FilePath) -- ^ The result
schedulesDir n = fmap (</> "schedules") <$> storageDir n

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

-- | A abstraction Monad to isolate real IO Actions
--
-- @since 0.1.0
class Monad m => MonadStorage m where
    -- A 'decodeFileEither' wrapper
    decodeFileEither' :: FromJSON a => FilePath -> m (Either ParseException a)

    -- A 'encodeFile' wrapper
    tryEncodeFile' :: ToJSON a => FilePath -> a -> m (Either IOError ())

    -- A 'try createDirectory' wrapper
    tryCreateDirectory' :: FilePath -> m (Either IOError ())

    -- A 'try createDirectoryIfMissing' wrapper
    tryCreateDirectoryIfMissing' :: Bool -> FilePath -> m (Either IOError ())

    -- A 'try getHomeDirectory' wrapper
    tryGetHomeDirectory' :: m (Either IOError FilePath)

    -- A 'try' yaml file globbing wrapper
    tryListYamlFiles' :: FilePath -> m (Either IOError [FilePath])

    -- A 'try removeDirectoryRecursive' wrapper
    tryRemoveDirectoryRecursive' :: FilePath -> m (Either IOError ())

    -- A 'try writeFile' wrapper
    tryWriteFile' :: FilePath -> String -> m (Either IOError ())

-- | The implementation of the isolation abstraction for the IO Monad
--
-- @since 0.1.0
instance MonadStorage IO where
    decodeFileEither' = decodeFileEither
    tryEncodeFile' p e = try $ encodeFile p e
    tryCreateDirectory' = try . createDirectory
    tryCreateDirectoryIfMissing' a f = try $ createDirectoryIfMissing a f
    tryListYamlFiles' f = try . flip globDir1 f . compile $ "*." ++ yamlExt
    tryGetHomeDirectory' = try getHomeDirectory
    tryRemoveDirectoryRecursive' = try . removeDirectoryRecursive
    tryWriteFile' f s = try $ writeFile f s

-- | Helper for monadic 'Either' standard handling
--
-- @since 0.1.0
(>>-)
    :: Monad m
    => m (Either a b)        -- ^ The monadic value to be unwrapped
    -> (b -> m (Either a c)) -- ^ The function to be applied if Either is 'Right'
    -> m (Either a c)        -- ^ The result
a >>- f = a >>= either (return . Left) f

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
    :: (MonadGit m, MonadStorage m)
    => String                -- ^ The name of the storage
    -> String                -- ^ The git command
    -> m (Either IOError ()) -- ^ The result
runGit n c = storageDir n >>- runGitCommandIO c

-- | Create all needed Storage directories for a given name
--
-- @since 0.1.0
createStorageDirs
    :: MonadStorage m
    => String                -- ^ The name of the storage
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
    => String                -- ^ The name of the storage
    -> m (Either IOError ()) -- ^ The result
createKeepFiles n = foldM
    (\_ f -> f n >>- (\p -> tryWriteFile' (p </> keepFile) ""))
    (Right ())
    [actionsDir, resourcesDir, schedulesDir]

-- | Create the global cache directory for all storages
--
-- @since 0.1.0
createCacheDir :: (MonadStorage m) => m (Either IOError ()) -- ^ The result
createCacheDir = cacheDir >>- tryCreateDirectoryIfMissing' True

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
    :: (MonadStorage m, MonadGit m)
    => String                -- ^ The name of the storage
    -> Maybe String          -- ^ The remote URL of the storage
    -> m (Either IOError ()) -- ^ The result
new n r =
    (   createCacheDir
        >>> createStorageDirs n
        >>> createKeepFiles n
        >>> runGit n "init"
        >>> runGit n "add ."
        >>> runGit n "commit -m 'Init'"
        >>> maybe
                (return $ Right ())
                ( \x -> runGit n ("remote add origin " ++ x)
                    >>> runGit n "push -u origin master"
                )
                r
        )
        >>= either (\l -> remove n >> return (Left l)) (return . Right)

-- | Remove a storage for the given name
--
-- @since 0.1.0
remove
    :: MonadStorage m
    => String                  -- ^ The name of the storage
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
    foldM (\a v -> loadFile v >>= (\r -> return ((:) <$> r <*> a))) (Right [])

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
    :: (MonadStorage m)
    => String                      -- ^ The name of the storage
    -> m (Either IOError [Action]) -- ^ The result
loadActions = loadEntities . actionsDir

-- | Load all Resources for the given name of the storage.
--
-- @since 0.1.0
loadResources
    :: (MonadStorage m)
    => String                        -- ^ The name of the storage
    -> m (Either IOError [Resource]) -- ^ The result
loadResources = loadEntities . resourcesDir

-- | Load all Schedules for the given name of the storage.
--
-- @since 0.1.0
loadSchedules
    :: (MonadStorage m)
    => String                        -- ^ The name of the storage
    -> m (Either IOError [Schedule]) -- ^ The result
loadSchedules = loadEntities . schedulesDir

-- | Save the given list of 'ToJSON a' to the 'FilePath'
--
-- @since 0.1.0
saveFiles
    :: (IsManifest a, ToJSON a, MonadStorage m)
    => [a]                   -- ^ The instances to be stored
    -> FilePath              -- ^ The location of the saved files
    -> m (Either IOError ()) -- ^ The result
saveFiles a p = foldM
    (\_ v -> tryEncodeFile' (p </> uuidString v <.> yamlExt) v)
    (Right ())
    a

-- | Save certain entities from a list into a directory
--
-- @since 0.1.0
saveEntities
    :: (IsManifest a, MonadStorage m, ToJSON a)
    => (t -> m (Either IOError FilePath))   -- ^ The target directory
    -> t                                    -- ^ The name of the storage
    -> [a]                                  -- ^ The entities to be stored
    -> m (Either IOError ())                -- ^ The result
saveEntities f n a = f n >>- saveFiles a

-- | Save all 'Action's for the given name of the storage.
--
-- @since 0.1.0
saveActions
    :: (MonadStorage m)
    => String                -- ^ The name of the storage
    -> [Action]              -- ^ The 'Action's to be stored
    -> m (Either IOError ()) -- ^ The result
saveActions = saveEntities actionsDir

-- | Save all 'Resource's for the given name of the storage.
--
-- @since 0.1.0
saveResources
    :: (MonadStorage m)
    => String                -- ^ The name of the storage
    -> [Resource]            -- ^ The 'Resource's to be stored
    -> m (Either IOError ()) -- ^ The result
saveResources = saveEntities resourcesDir

-- | Save all 'Schedule's for the given name of the storage.
--
-- @since 0.1.0
saveSchedules
    :: (MonadStorage m)
    => String                -- ^ The name of the storage
    -> [Schedule]            -- ^ The 'Schedule's to be stored
    -> m (Either IOError ()) -- ^ The result
saveSchedules = saveEntities schedulesDir
