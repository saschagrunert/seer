-- | This module includes everything related to Storage handling.
--
-- @since 0.1.0

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Seer.Storage (
    MonadStorage,
    Storage,
    actions,
    empty,
    load,
    resources,
    save,
) where

import Data.Bifunctor (first)
import Data.Yaml
import Seer.Action (Actions)
import Seer.Resource (Resources)

-- | A abstraction Monad to isolate real IO Actions
--
-- @since 0.1.0
class Monad m => MonadStorage m where
    -- A 'decodeFileEither' wrapper
    decodeFileEither' :: FilePath -> m (Either ParseException Storage)

    -- A 'encodeFile' wrapper
    encodeFile' :: ToJSON a => FilePath -> a -> m ()

-- | The implementation of the isolation abstraction for the IO Monad
--
-- @since 0.1.0
instance MonadStorage IO where
    decodeFileEither' = decodeFileEither
    encodeFile' = encodeFile

-- | The storage for representing 'Resources' and 'Actions'.
--
-- @since 0.1.0
data Storage = Storage { resources :: Maybe Resources -- ^ The resources to be stored
                       , actions :: Maybe Actions -- ^ The actions to be stored
                       } deriving (Eq, Show)

-- | Parses the 'Storage' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON Storage where
    parseJSON (Object m) = Storage <$>
        m .: "resources" <*>
        m .: "actions"
    parseJSON x = fail $ "not an object: " ++ show x

-- | Generates the YAML/JSON from a 'Storage'
--
-- @since 0.1.0
instance ToJSON Storage where
    toJSON Storage{..} = object [ "resources" .= resources
                                , "actions" .= actions ]

-- | The empty 'Storage' representation
--
-- Examples:
--
-- >>> empty
-- Storage {resources = Nothing, actions = Nothing}
--
-- @since 0.1.0
empty :: Storage
empty = Storage {resources = Nothing, actions = Nothing}

-- | Load from the given 'FilePath' and return either a error 'String' or a
-- valid 'Storage' instance
--
-- Examples:
--
-- >>> load "file_not_existing"
-- Left "YAML exception:\nYaml file not found: file_not_existing"
--
-- >>> :m +Data.Either
-- >>> isRight <$> load "test/files/example.yaml"
-- True
--
-- @since 0.1.0
load
    :: MonadStorage m
    => FilePath                  -- ^ The location of the file to be parsed
    -> m (Either String Storage) -- ^ The 'Either' result
load a = first prettyPrintParseException <$> decodeFileEither' a

-- | Save the given 'Storage' to the 'FilePath'
--
-- Examples:
--
-- >>> save empty ".testfile"
--
-- @since 0.1.0
save
    :: MonadStorage m
    => Storage        -- ^ The storage instance to be serialized
    -> FilePath       -- ^ The location of the file to be saved
    -> m ()           -- ^ The function should succeed anyway
save = flip encodeFile'
