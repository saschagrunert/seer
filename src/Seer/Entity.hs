-- | This module includes everything about general entity handling
--
-- @since 0.1.0

{-# LANGUAGE MultiParamTypeClasses  #-}

module Seer.Entity (
    EntityRelation(..)
) where

-- | A relation between a single type `s` and its plural `p`
--
-- @since 0.1.0
class EntityRelation s p where
    -- | Adds a single `s` to plural `p`
    --
    -- @since 0.1.0
    infixr 5 |+
    (|+)
        :: p -- ^ The plural where the single entity should be added
        -> s -- ^ The single entity to be added
        -> p -- ^ The resulting plural

    -- | Removes a single `s` from plural `p`
    --
    -- @since 0.1.0
    infixr 5 |-
    (|-)
        :: p -- ^ The plural where the single entity should be removed
        -> s -- ^ The single entity to be removed
        -> p -- ^ The resulting plural
