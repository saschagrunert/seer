-- | This module includes everything about pretty printing entities and their
-- relation.
--
-- @since 0.1.0

module Seer.View
  ( createView
  ) where

import           Control.Lens            ((^.))
import           Control.Monad           (join
                                         ,sequence)
import           Data.List               (intercalate)
import qualified Data.Map.Strict    as M (Map
                                         ,fromList
                                         ,toAscList
                                         ,unionsWith
                                         ,unionWith)
import           Data.Maybe              (isNothing)
import           Data.Time.Clock         (UTCTime
                                         ,utctDay)
import           Data.Time.Format        (FormatTime
                                         ,defaultTimeLocale
                                         ,formatTime)
import           Data.Time.Calendar      (Day)
import qualified Seer.Action        as A (Action
                                         ,duration
                                         ,name)
import           Seer.Manifest           (metadata
                                         ,spec
                                         ,uid)
import qualified Seer.Resource      as R (Resource
                                         ,availabilities
                                         ,name)
import qualified Seer.Schedule      as S (Schedule
                                         ,actionID
                                         ,resourceID
                                         ,start)
import           Seer.Time               (Availabilities
                                         ,evaluateDaily)
import           Seer.Utils              (rstrip)
import           Text.Printf             (printf)

-- | The representation of a 'View'
--
-- @since 0.1.0
type View = M.Map Day (M.Map String [Entry])

-- | A single Entry
--
-- @since 0.1.0
data Entry = Entry { resource :: String               -- ^ The name of the 'Resource'
                   , action :: String                 -- ^ The name of the 'Action'
                   , availabilities :: Availabilities -- ^ The Availabilities
                   , end :: String                    -- ^ The proposed end time
                   , entryType :: EntryType           -- ^ The entry type
                   , showResource :: Bool             -- ^ If the Resource should be shown
                   , showAction :: Bool               -- ^ If the Action should be shown
                   } deriving (Eq, Ord)

-- | A type of the Entry
--
-- @since 0.1.0
data EntryType = Start -- ^ A starting entry
               | End   -- ^ A ending entry
               deriving (Eq, Ord)

-- | Shows an Entry
--
-- @since 0.1.0
instance Show Entry where
  show (Entry r a _ _ Start sr sa)
    | sr && sa = printf "%s %s (%s)" sc a r
    | not sr && sa = printf "%s %s" sc a
    | sr && not sa = printf "%s %s" sc r
    | otherwise = sc
    where sc = "↦"
  show (Entry r a _ _ End sr sa)
    | sr && sa = printf "%s %s (%s)" ec a r
    | not sr && sa = printf "%s %s" ec a
    | sr && not sa = printf "%s %s" ec r
    | otherwise = ec
    where ec = "⇥"

-- | The default date formatting output
--
-- @since 0.1.0
dayFormat :: FormatTime a => a -> String
dayFormat = formatTime defaultTimeLocale "%a [%d.%m]"

-- | Get a new View for a list of 'Schedule's, 'Resource's, and 'Action's
--
-- @since 0.1.0
createView
  :: [S.Schedule] -- ^ The Schedules
  -> [R.Resource] -- ^ The Resources
  -> [A.Action]   -- ^ The Actions
  -> UTCTime      -- ^ The start date and time
  -> UTCTime      -- ^ The end date and time
  -> Maybe String -- ^ The resource to be filtered
  -> Maybe String -- ^ The action to be filtered
  -> Maybe String -- ^ The result
createView sx ax rx s e rf af =
  toString . M.unionsWith (M.unionWith (++)) <$> join
    (mapM (getViewSingleton s e rf af) <$> getTuple sx ax rx)

-- | Convert a View to a String
--
-- @since 0.1.0
toString :: View -> String
toString v =
  intercalate "\n"
    $   ( \(d, xs) ->
          printf "%s %s" (dayFormat d)
            .   rstrip
            .   unwords
            $   (\(t, ex) -> printf "%s %s\n%s" t (ets ex) (spaces dateIndent))
            <$> M.toAscList xs
        )
    <$> M.toAscList v
 where
  ets ex = intercalate (printf "\n%s" $ spaces actionIndent) $ show <$> ex
  spaces x = replicate x ' '
  dateIndent   = 11
  actionIndent = dateIndent + 7

-- | Get the tuple relation between 'Schedule', 'Resource' and 'Action'
--
-- @since 0.1.0
getTuple
  :: [S.Schedule]
  -> [R.Resource]
  -> [A.Action]
  -> Maybe [(S.Schedule, R.Resource, A.Action)]
getTuple sx rx ax =
  sequence $ (\s -> reduce (s, getResource s, getAction s)) <$> sx
 where
  findPerUid y s xs =
    case filter (\x -> x ^. metadata . uid == y ^. spec . s) xs of
      [e] -> Just e
      _   -> Nothing
  getAction s = findPerUid s S.actionID ax
  getResource s = findPerUid s S.resourceID rx
  reduce (x, Just y, Just z) = Just (x, y, z)
  reduce _                   = Nothing

-- | Create singleton View From a start and end time, resource and action to be
-- filtered and a tuple of 'Schedule', 'Resource' and 'Action'
--
-- @since 0.1.0
getViewSingleton
  :: UTCTime       -- ^ The start time
  -> UTCTime       -- ^ The end time
  -> Maybe String  -- ^ The resource to be filtered
  -> Maybe String  -- ^ The action to be filtered
  -> (S.Schedule, R.Resource, A.Action)
  -> Maybe View
getViewSingleton startTime endTime rf af (s, r, a) =
  M.fromList
    .   map
          ( \(x, y, z) ->
            (x, M.fromList [(y, pure $ su z Start), (z, pure $ su z End)])
          )
    .   filter
          ( \(d, _, _) ->
            startDay
              <= d
              && d
              <= endDay
              && filterMaybe rf r R.name
              && filterMaybe af a A.name
          )
    <$> evaluateDaily st av du
 where
  st = s ^. spec . S.start
  av = r ^. spec . R.availabilities
  du = a ^. spec . A.duration
  su z = newEntry r a av z (isNothing rf) (isNothing af)
  startDay = utctDay startTime
  endDay   = utctDay endTime
  filterMaybe (Just x) y z = x == y ^. spec . z
  filterMaybe _        _ _ = True

-- | Create a new 'Subject' from a given 'Resource' and 'Action'
--
-- @since 0.1.0
newEntry
  :: R.Resource
  -> A.Action
  -> Availabilities
  -> String
  -> Bool
  -> Bool
  -> EntryType
  -> Entry
newEntry r a av e sr sa s = Entry
  { resource       = r ^. spec . R.name
  , action         = a ^. spec . A.name
  , availabilities = av
  , end            = e
  , entryType      = s
  , showResource   = sr
  , showAction     = sa
  }
