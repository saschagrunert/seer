-- | This module includes everything related to Storage handling.
--
-- @since 0.1.0

module Seer.Storage (
    MonadStorage,
    load,
    save,
) where

import Data.Bifunctor (first)
import Data.Yaml

-- | A abstraction Monad to isolate real IO Actions
--
-- @since 0.1.0
class Monad m => MonadStorage m where
    -- A 'decodeFileEither' wrapper
    decodeFileEither' :: FromJSON a => FilePath -> m (Either ParseException a)

    -- A 'encodeFile' wrapper
    encodeFile' :: ToJSON a => FilePath -> a -> m ()

-- | The implementation of the isolation abstraction for the IO Monad
--
-- @since 0.1.0
instance MonadStorage IO where
    decodeFileEither' = decodeFileEither
    encodeFile' = encodeFile

-- | Load from the given 'FilePath' and return either a error 'String' or a
-- valid 'Storage' instance
--
-- @since 0.1.0
load
    :: (MonadStorage m, FromJSON a)
    => FilePath            -- ^ The location of the file to be parsed
    -> m (Either String a) -- ^ The 'Either' result
load a = first prettyPrintParseException <$> decodeFileEither' a

-- | Save the given 'Storage' to the 'FilePath'
--
-- @since 0.1.0
save
    :: (MonadStorage m, ToJSON a)
    => a        -- ^ The storage instance to be serialized
    -> FilePath -- ^ The location of the file to be saved
    -> m ()     -- ^ The function should succeed anyway
save = flip encodeFile'
