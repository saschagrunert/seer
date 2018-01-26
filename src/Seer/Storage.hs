-- | This module includes everything related to Storage handling.
--
-- @since 0.1.0

module Seer.Storage (
    MonadStorage,
    loadFile,
    new,
    remove,
    saveFile,
) where

import Control.Exception (try)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Yaml (FromJSON(..)
                 ,ParseException
                 ,ToJSON(..)
                 ,decodeFileEither
                 ,encodeFile
                 ,prettyPrintParseException)
import Seer.Git (MonadGit, runGitCommandIO)
import System.Directory (createDirectory
                        ,createDirectoryIfMissing
                        ,getHomeDirectory
                        ,removeDirectoryRecursive)
import System.FilePath.Posix ((</>))
import System.IO.Error (IOError)

-- | The global storage cache directory
--
-- @since 0.1.0
cacheDir :: MonadStorage m => m (Either IOError FilePath)
cacheDir = fmap (</> ".seer" </> "cache") <$> try' getHomeDirectory'

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
actionsDir :: FilePath
actionsDir = "actions"

-- | The storage internal global 'Resources' directory
--
-- @since 0.1.0
resourcesDir :: FilePath
resourcesDir = "resources"

-- | The storage internal global 'Schedules' directory
--
-- @since 0.1.0
schedulesDir :: FilePath
schedulesDir = "schedules"

-- | A abstraction Monad to isolate real IO Actions
--
-- @since 0.1.0
class Monad m => MonadStorage m where
    -- A 'createDirectory' wrapper
    createDirectory' :: FilePath -> m ()

    -- A 'createDirectoryIfMissing' wrapper
    createDirectoryIfMissing' :: Bool -> FilePath -> m ()

    -- A 'decodeFileEither' wrapper
    decodeFileEither' :: FromJSON a => FilePath -> m (Either ParseException a)

    -- A 'encodeFile' wrapper
    encodeFile' :: ToJSON a => FilePath -> a -> m ()

    -- A 'getHomeDirectory' wrapper
    getHomeDirectory' :: m FilePath

    -- A 'removeDirectoryRecursive' wrapper
    removeDirectoryRecursive' :: FilePath -> m ()

    -- A 'try' wrapper
    try' :: m a -> m (Either IOError a)

-- | The implementation of the isolation abstraction for the IO Monad
--
-- @since 0.1.0
instance MonadStorage IO where
    createDirectory' = createDirectory
    createDirectoryIfMissing' = createDirectoryIfMissing
    decodeFileEither' = decodeFileEither
    encodeFile' = encodeFile
    getHomeDirectory' = getHomeDirectory
    removeDirectoryRecursive' = removeDirectoryRecursive
    try' = try

-- | Helper for monadic 'Either' standard handling
--
-- @since 0.1.0
(>>-)
    :: Monad m
    => m (Either a b)        -- ^ The monadic value to be unwrapped
    -> (b -> m (Either a c)) -- ^ The function to be applied if Either is 'Right'
    -> m (Either a c)        -- ^ The result
a >>- f = a >>= either (return . Left) f

-- | Helper for monadic 'Either' error handling
--
-- @since 0.1.0
(>>|)
    :: MonadStorage m
    => m (Either IOError a) -- ^ The monadic value to be unwrapped
    -> (a -> m b)           -- ^ The function which will be applied with 'try'
    -> m (Either IOError b) -- ^ The result
a >>| f = a >>- (try' . f)

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
runGitCommandInStorageDir
    :: (MonadGit m, MonadStorage m)
    => String                -- ^ The name of the storage
    -> String                -- ^ The git command
    -> m (Either IOError ()) -- ^ The result
runGitCommandInStorageDir n c = storageDir n >>- runGitCommandIO c

-- | Create all needed Storage directories for a given name
--
-- @since 0.1.0
createStorageDirs
    :: MonadStorage m
    => String                -- ^ The name of the storage
    -> m (Either IOError ()) -- ^ The result
createStorageDirs n =
    storageDir n
        >>| ( \a -> foldM
                (\_ v -> createDirectory' v)
                ()
                [a, a </> actionsDir, a </> schedulesDir, a </> resourcesDir]
            )

-- | Create the global cache directory for all storages
--
-- @since 0.1.0
createCacheDir :: (MonadStorage m) => m (Either IOError ()) -- ^ The result
createCacheDir = cacheDir >>| createDirectoryIfMissing' True

-- | Create a new storage for the given name
--
-- @since 0.1.0
new
    :: (MonadStorage m, MonadGit m)
    => String                -- ^ The name of the storage
    -> String                -- ^ The remote URL of the storage
    -> m (Either IOError ()) -- ^ The result
new n u =
    createCacheDir
        >>> createStorageDirs n
        >>> runGitCommandInStorageDir n "init"
        >>> runGitCommandInStorageDir n ("remote add origin " ++ u)

-- | Remove a storage for the given name
--
-- @since 0.1.0
remove
    :: MonadStorage m
    => String                  -- ^ The name of the storage
    -> m (Either IOError ())   -- ^ The result
remove n = storageDir n >>| removeDirectoryRecursive'

-- | Load from the given 'FilePath' and return either a error 'String' or a
-- valid instance of 'FromJSON a'
--
-- @since 0.1.0
loadFile
    :: (MonadStorage m, FromJSON a)
    => FilePath            -- ^ The location of the file to be parsed
    -> m (Either String a) -- ^ The result
loadFile a = first prettyPrintParseException <$> decodeFileEither' a

-- | Save the given 'ToJSON a' to the 'FilePath'
--
-- @since 0.1.0
saveFile
    :: (MonadStorage m, ToJSON a)
    => a        -- ^ The storage instance to be serialized
    -> FilePath -- ^ The location of the file to be saved
    -> m ()     -- ^ The result
saveFile = flip encodeFile'
