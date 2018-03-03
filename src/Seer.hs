-- | This module everything related to the main library interface
--
-- @since 0.1.0

module Seer
  ( Error
  , MonadSeer
  , createAction
  , createResource
  , createSchedule
  , createStorage
  , deleteAction
  , deleteResource
  , deleteSchedule
  , deleteStorage
  , getActions
  , getConfig
  , getResources
  , getSchedules
  , getStorages
  , setDefaultStorage
  , version
  ) where

import           Control.Monad          (mapM
                                        ,(>=>))
import           Data.Bifunctor         (bimap
                                        ,second)
import           Data.List              (isInfixOf
                                        ,sortBy
                                        ,transpose)
import           Data.Time.Clock        (UTCTime
                                        ,getCurrentTime)
import           Data.UUID              (UUID)
import qualified Seer.Action as A       (Action
                                        ,ActionSpec(name)
                                        ,duration
                                        ,new)
import qualified Seer.Config as C       (Config
                                        ,new
                                        ,storage)
import           Seer.Manifest          (Manifest
                                        ,ToList (headers
                                                ,toList)
                                        ,creationTimestamp
                                        ,metadata
                                        ,spec
                                        ,uid)
import qualified Seer.Resource as R     (Resource
                                        ,ResourceSpec(name)
                                        ,availabilities
                                        ,new)
import qualified Seer.Schedule as S     (Schedule
                                        ,ScheduleSpec(actionID
                                                     ,resourceID
                                                     ,start)
                                        ,new)
import           Seer.Storage           (Storage
                                        ,list
                                        ,loadActions
                                        ,loadConfig
                                        ,loadResources
                                        ,loadSchedules
                                        ,new
                                        ,remove
                                        ,removeActions
                                        ,removeResources
                                        ,removeSchedules
                                        ,save
                                        ,saveActions
                                        ,saveConfig
                                        ,saveResources
                                        ,saveSchedules
                                        ,storageExist)
import           Seer.Time              (Availabilities
                                        ,WeekDay(Mon
                                                ,Tue
                                                ,Wed
                                                ,Thu
                                                ,Fri
                                                ,Sat
                                                ,Sun)
                                        ,dateTimeFormat
                                        ,evaluateEnd
                                        ,evaluateStart
                                        ,parseDateTime
                                        ,utcToLocal
                                        ,weekAvailableFromTo)
import           Seer.Utils             ((>>-)
                                        ,rstrip)
import           System.IO.Error        (ioeGetErrorString)
import           Text.PrettyPrint.Boxes (hsep
                                        ,left
                                        ,render
                                        ,text
                                        ,vcat)
import           Text.Printf            (printf)

-- | The version of the library
--
-- @since 0.1.0
version :: String
version = "0.1.0"

-- | A abstraction Monad to isolate real IO Actions
--
-- @since 0.1.0
class Monad m => MonadSeer m where
  getCurrentTime' :: m UTCTime
  getLine' :: m String
  list' :: m (Either IOError [[String]])
  loadActions' ::  Storage -> m (Either IOError [A.Action])
  loadConfig' :: m (Either IOError C.Config)
  loadResources' ::  Storage -> m (Either IOError [R.Resource])
  loadSchedules' ::  Storage -> m (Either IOError [S.Schedule])
  log' :: String -> m ()
  newAction' :: String -> Maybe String -> String -> m (Maybe A.Action)
  newConfig' :: String -> m C.Config
  newResource' :: String -> Maybe String -> Availabilities -> m R.Resource
  newSchedule' :: UTCTime -> UUID -> UUID -> m S.Schedule
  newStorage' :: Storage -> Maybe String -> m (Either IOError ())
  removeActions' :: Storage -> [A.Action] -> m (Either IOError ())
  removeResources' :: Storage -> [R.Resource] -> m (Either IOError ())
  removeSchedules' :: Storage -> [S.Schedule] -> m (Either IOError ())
  removeStorage' :: Storage -> m (Either IOError ())
  save' :: Storage -> m (Either IOError ())
  saveActions' :: Storage -> [A.Action] -> m (Either IOError ())
  saveConfig' :: C.Config -> m (Either IOError ())
  saveResources' :: Storage -> [R.Resource] -> m (Either IOError ())
  saveSchedules' :: Storage -> [S.Schedule] -> m (Either IOError ())
  storageExist' :: Storage -> m (Either IOError Bool)
  utcToLocal' :: UTCTime -> m String

-- | The implementation of the isolation abstraction for the IO Monad
--
-- @since 0.1.0
instance MonadSeer IO where
  getCurrentTime' = getCurrentTime
  getLine' = getLine
  list' = list
  loadActions' = loadActions
  loadConfig' = loadConfig
  loadResources' = loadResources
  loadSchedules' = loadSchedules
  log' = putStrLn
  newAction' = A.new
  newConfig' = C.new
  newResource' = R.new
  newSchedule' = S.new
  newStorage' = new
  removeActions' = removeActions
  removeSchedules' = removeSchedules
  removeResources' = removeResources
  removeStorage' = remove
  save' = save
  saveActions' = saveActions
  saveConfig' = saveConfig
  saveResources' = saveResources
  saveSchedules' = saveSchedules
  storageExist' = storageExist
  utcToLocal' = utcToLocal

-- | A reference to an Error Message Storage
--
-- @since 0.1.0
type Error = String

-- | Convert an IOError to an 'Error' on the Left side of an 'Either' type and
-- provide an alternative function
--
-- @since 0.1.0
eitherToErrorOr
  :: (MonadSeer m)
  => (a -> m (Either Error b))
  -> Either IOError a
  -> m (Either Error b)
eitherToErrorOr = either leftError

-- | Convert an IOError to an 'Error' on the Left side of an 'Either' type
--
-- @since 0.1.0
leftError
  :: MonadSeer m
  => IOError            -- ^ The error to be processed
  -> m (Either Error b) -- ^ The result
leftError e = return . Left $ ioeGetErrorString e

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

-- | Maps an 'bimapIOError' including a default done message
--
-- @since 0.1.0
bimapIOErrorDone :: Functor f => f (Either IOError b) -> f (Either Error String)
bimapIOErrorDone f = bimapIOError (const done) <$> f

-- | The 'maybe' function with a default 'Error' to the 'Left'
--
-- @since 0.1.0
maybeError
  :: MonadSeer m
  => Error                     -- ^ The 'Error' in failure case
  -> (c -> m (Either Error b)) -- ^ The function passed to the 'maybe'
  -> Maybe c                   -- ^ The maybe value to be evaluated
  -> m (Either Error b)        -- ^ The result
maybeError e = maybe . return $ Left e

-- | Maps a String to a Maybe value. Evaluates to Nothing if the String is
-- empty.
--
-- @since 0.1.0
maybeString :: String -> Maybe String
maybeString "" = Nothing
maybeString s  = Just s

-- | Creates a string table from a two dimensional String list
--
-- @since 0.1.0
tablify
  :: [[String]] -- ^ The list list to be formatted
  -> String     -- ^ The result
tablify r =
  rstrip . render . hsep 2 left $ vcat left . map text <$> transpose r

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
    then bimapIOErrorDone $ newConfig' n >>= saveConfig'
    else return $ Left "Storage does not exist"

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
  bimapIOError (\r -> if null r then nf else tablify $ ["NAME", "REMOTE"] : r)
    <$> list'

-- | List the current Configuration if possible. Evaluates to either a fully
-- formatted table (Right) or an error message (Left).
--
-- @since 0.1.0
getConfig :: MonadSeer m => m (Either Error String)
getConfig = loadConfig' >>| (fmap Right . toListsLocal . pure)

-- | Function for receiving an entity and formatting the output as String table
--
-- @since 0.1.0
getAndTabilifyEntity
  :: (ToList a, MonadSeer m)
  => (String -> m (Either IOError [Manifest a])) -- ^ The entity retrieval function
  -> m (Either Error String)                     -- ^ The result
getAndTabilifyEntity f =
  getDefaultStorage
    >>| (f >=> eitherToErrorOr (toListsLocal >=> return . Right))

-- | Takes a list of entities and turns them into a tablified version including
-- the local creation time
--
-- @since 0.1.0
toListsLocal
  :: (ToList a, MonadSeer m)
  => [Manifest a] -- ^ The Manifests to be converted
  -> m String     -- ^ The result
toListsLocal a@(_:_) = tablify . (:) h <$> mapM
  t
  (zip [1 :: Int ..] (sortBy sortCreated a))
 where
  t (i, e) =
    utcToLocal' (creationTimestamp $ metadata e)
      >>= (\l -> return $ show i : toList e ++ [l])
  h = ["#"] ++ headers (head a) ++ ["CREATED"]
  sortCreated x y =
    if creationTimestamp (metadata x) > creationTimestamp (metadata y)
      then LT
      else GT
toListsLocal _ = return nf

-- | List all available Actions for the default Storage. Evaluates to either a
-- fully formatted table (Right) or an error message (Left).
--
-- @since 0.1.0
getActions :: MonadSeer m => m (Either Error String)
getActions = getAndTabilifyEntity loadActions'

-- | List all available Resources for the default Storage. Evaluates to either
-- a fully formatted table (Right) or an error message (Left).
--
-- @since 0.1.0
getResources :: MonadSeer m => m (Either Error String)
getResources = getAndTabilifyEntity loadResources'

-- | List all available Schedules for the default Storage. Evaluates to either
-- a fully formatted table (Right) or an error message (Left). The second
-- argument specifies if all schedules or just the future ones should be shown.
--
-- @since 0.1.0
getSchedules
  :: MonadSeer m
  => Bool                    -- ^ True if also past schedules should be shown
  -> m (Either Error String) -- ^ The result
getSchedules showAll =
  getDefaultStorage
    >>| ( \d -> do
          sx <- loadSchedules' d
          rx <- loadResources' d
          ax <- loadActions' d
          t  <- getCurrentTime'
          case (sx, rx, ax) of
            (Left e, _     , _     ) -> leftError e
            (_     , Left e, _     ) -> leftError e
            (_     , _     , Left e) -> leftError e
            (Right s, Right r, Right a) ->
              rows r a s t
                >>= ( \k -> return . Right $ if null k
                                             then
                                               nf
                                             else
                                               tablify $ header : k
                    )
        )
 where
  header = ["#", "FROM", "TO", "DURATION", "RESOURCE", "ACTION", "CREATED"]
  rows r a s t = mapM
    ( \(i, v) -> do
      cr <- utcToLocal' (creationTimestamp $ metadata v)
      let fr = dateTimeFormat $ start v
      let ru = S.resourceID $ spec v
      let au = S.actionID $ spec v
      let l  = [show i, fr]
      case (fuid ru r, fuid au a) of
        (x:_, y:_) -> return $ l ++ [end v x y, adura y, rname x, aname y, cr]
        (_  , y:_) -> return $ l ++ [n, adura y, n, aname y, cr]
        (x:_, _  ) -> return $ l ++ [n, n, rname x, n, cr]
        (_  , _  ) -> return $ l ++ [n, n, n, n, cr]
    )
    (zip [1 :: Int ..] . filter (past t) $ sortBy sortFrom s)
  past t x = showAll || start x >= t
  sortFrom x y = if start x > start y then GT else LT
  fuid a = filter (\x -> uid (metadata x) == a)
  end v x y = maybe "" dateTimeFormat
    $ evaluateEnd (start v) (R.availabilities $ spec x) (A.duration $ spec y)
  n = "✗ not available"
  start x = S.start $ spec x
  rname = R.name . spec
  aname = A.name . spec
  adura = show . A.duration . spec

-- | Create a new Storage and evaluate to an Error if anything went wrong.
--
-- @since 0.1.0
createStorage
  :: MonadSeer m
  => Storage                 -- ^ The name of the storage
  -> String                  -- ^ The remote, can be empty
  -> m (Either Error String) -- ^ The result
createStorage n r =
  newStorage' n (maybeString r) >>| const (setDefaultStorage n)

-- | A convenience save and sync. Takes the function 'f', which should save the
-- entity and syncs the default storage afterwards.
--
-- @since 0.1.0
saveAndSync
  :: (Applicative f, MonadSeer m)
  => (String -> f a -> m (Either IOError b)) -- ^ The entity save function
  -> a                                       -- ^ The entity to be saved
  -> m (Either Error String)                 -- ^ The result
saveAndSync f x = getDefaultStorage >>| (\s -> f s (pure x) >>| sync s)

-- | A convenience remove and sync.
--
-- @since 0.1.0
removeAndSync
  :: MonadSeer m
  => (String -> t -> m (Either IOError a)) -- ^ The removal function
  -> t                                     -- ^ The element to be removed
  -> m (Either Error String)               -- ^ The result
removeAndSync f x = getDefaultStorage >>| (\s -> f s x >>| sync s)

-- | A convenience sync.
--
-- @since 0.1.0
sync :: MonadSeer m => Storage -> a -> m (Either Error String)
sync s = const . bimapIOErrorDone $ save' s

-- | Create a new 'Action' for a given name, description and duration.
--
-- @since 0.1.0
createAction
  :: MonadSeer m
  => String                  -- ^ The name of the Action
  -> String                  -- ^ The description, can be empty
  -> String                  -- ^ The duration as a String
  -> m (Either Error String) -- ^ The result
createAction n d r = newAction' n (maybeString d) r
  >>= maybeError "Unable to create Action" (saveAndSync saveActions')

-- | Create a new 'Resource' for a given name, description and available times.
-- The available times in the list needs to be in the order from Monday to
-- Sunday, whereas empty Strings are allowed.
--
-- @since 0.1.0
createResource
  :: MonadSeer m
  => String                                                   -- ^ The name of the Action
  -> String                                                   -- ^ The description, can be empty
  -> (String, String, String, String, String, String, String) -- ^ The time span Strings
  -> m (Either Error String)                                  -- ^ The result
createResource n d a =
  maybeError "Unable to parse time spans"
             (newResource' n (maybeString d) >=> saveAndSync saveResources')
    $ toAvailability a
 where
  toAvailability (m, t, w, h, f, s, u) =
    weekAvailableFromTo
      $  i Mon m
      ++ i Tue t
      ++ i Wed w
      ++ i Thu h
      ++ i Fri f
      ++ i Sat s
      ++ i Sun u
  i _ "" = []
  i x y  = pure $ (,) x y

-- | Filter an entity by a given filter function
--
-- @since 0.1.0
filterEntity
  :: String         -- ^ The filter string
  -> (b -> String)  -- ^ The filter applicator
  -> [Manifest b]   -- ^ The list to be filtered
  -> [Manifest b]   -- ^ The result
filterEntity n l = filter $ contains n l
  where contains x y = isInfixOf x . y . spec

-- | Create a new 'Schedule' for two given date strings (from/to), an resource
-- and and action name.
--
-- @since 0.1.0
createSchedule
  :: MonadSeer m
  => String                   -- ^ The date from where the schedule should start
  -> String                   -- ^ The Resource name
  -> String                   -- ^ The Action name
  -> m (Either String String) -- ^ The result
createSchedule f r a =
  getDefaultStorage
    >>| ( \dir -> do
          resources <- loadResources' dir
          actions   <- loadActions' dir
          case parseDateTime f of
            Nothing         -> leftParseError f
            Just parsedDate -> case (resources, actions) of
              (Left e, _     ) -> leftError e
              (_     , Left e) -> leftError e
              (Right allResources, Right allActions) ->
                case
                    ( filterEntity r R.name allResources
                    , filterEntity a A.name allActions
                    )
                  of
                    ([], _ ) -> leftNotFound rs r
                    (_ , []) -> leftNotFound as a
                    (rx, ax) -> do
                      cr <- chooseOneOf rs rx
                      ca <- chooseOneOf as ax
                      case (cr, ca) of
                        (Nothing, _) -> le $ printf "Wrong input for the %s" rs
                        (_, Nothing) -> le $ printf "Wrong input for the %s" as
                        (Just resource, Just action) ->
                          buildSchedule dir parsedDate resource action
        )
 where
  leftParseError x = return . Left $ printf "Unable to parse date '%s'" x
  as   = "Action"
  rs   = "Resource"
  uuid = uid . metadata
  isInRange (Just x) y (Just z) | x >= y && x <= z = True
                                | otherwise        = False
  isInRange _ _ _ = False
  startDate x = S.start $ spec x
  endDate x y z =
    evaluateEnd x (R.availabilities $ spec y) (A.duration $ spec z)
  leftNotFound x y = le $ printf "No %s found for '%s'" x y
  le x = return $ Left x
  buildSchedule d p re ac =
    case evaluateStart p (R.availabilities $ spec re) of
      Nothing -> le "Unable calculate start date"
      Just from ->
        let
          end = endDate from re ac
        in
          logTime from end
          >>  loadSchedules' d
          >>| ( \s ->
                if not
                   .   or
                   $   ( \x -> isInRange end
                                         (startDate x)
                                         (endDate (startDate x) re ac)
                       )
                   <$> filter ((==) (uuid re) . S.resourceID . spec) s
                then
                  newSchedule' from (uuid re) (uuid ac)
                    >>= saveAndSync saveSchedules'
                else
                  le "The action is not schedulable at this time"
              )
  logTime x (Just y) = log' $ printf "Using calculated date range: %s - %s"
                                     (dateTimeFormat x)
                                     (dateTimeFormat y)
  logTime x _ =
    log' . printf "Unable to calculate end time for start: %s" $ dateTimeFormat
      x

-- | An generic entity chooser, displays the entities as a well formatted table
-- and lets the user interactively choose one item. Evaluates to 'Nothing' if
-- the number cannot be parsed correctly or is out of scope of the list.
--
-- @since 0.1.0
chooseOneOf
  :: (MonadSeer m, ToList a)
  => String                 -- ^ The entity name
  -> [Manifest a]           -- ^ The list of items to be displayed
  -> m (Maybe (Manifest a)) -- ^ The result
chooseOneOf _ [x] = return $ Just x
chooseOneOf a xs  = do
  lxs <- toListsLocal xs
  log' $ printf "Choose the %s by number:" a
  log' lxs
  h <- getLine'
  case reads h :: [(Int, String)] of
    (i, _):_ -> return . itemOf $ i - 1
    _        -> return Nothing
 where
  itemOf i | i >= l || i < 0 = Nothing
           | otherwise       = Just (xs !! i)
  l = length xs

-- | Delete a Storage by a given name.
--
-- @since 0.1.0
deleteStorage
  :: MonadSeer m
  => String                   -- ^ The name of the Storage
  -> m (Either Error String)  -- ^ The result
deleteStorage = bimapIOErrorDone . removeStorage'

-- | A generic entity deletion function.
--
-- @since 0.1.0
deleteEntity
  :: MonadSeer m
  => (String -> m (Either IOError a))    -- ^ A entity load function
  -> (String -> a -> m (Either Error b)) -- ^ A filter and delete function
  -> m (Either Error b)                  -- ^ The result
deleteEntity l f = getDefaultStorage >>| (\d -> l d >>| f d)

-- | Delete a 'Schedule' by a given start time.
--
-- @since 0.1.0
deleteSchedule
  :: MonadSeer m
  => String                   -- ^ The start date of the Schedule
  -> m (Either Error String)  -- ^ The result
deleteSchedule s = deleteEntity loadSchedules' filterAndDelete
 where
  filterAndDelete _ sx =
    case filter ((==) s . dateTimeFormat . S.start . spec) sx of
      []  -> return . Left $ printf "No Schedule found for start date '%s'" s
      x:_ -> removeAndSync removeSchedules' $ pure x

-- | A generic entity deletion function for filtering by their name
--
-- @since 0.1.0
filterAndDeleteByName
  :: (Applicative f, MonadSeer m, ToList a1)
  => String                   -- ^ The name to be removed
  -> (a1 -> String)           -- ^ The field to be filtered
  -> (S.ScheduleSpec -> UUID) -- ^ The uuid retrieval function
  -> String                   -- ^ The name of the entity type
  -> (String -> f (Manifest a1) -> m (Either IOError a2)) -- ^ The entity removal function
  -> Storage                  -- ^ The storage to be changed
  -> [Manifest a1]            -- ^ The entity to be filtered
  -> m (Either Error String)  -- ^ The result
filterAndDeleteByName s f i n r d xs = case filterEntity s f xs of
  []  -> return . Left $ printf "No %s found for name '%s'" n s
  [x] -> rmAndSync x
  es  -> chooseOneOf n es >>= maybeError "Got wrong input" rmAndSync
 where
  rmAndSync x =
    loadSchedules' d
      >>| ( removeAndSync removeSchedules'
          . filter ((==) (uid $ metadata x) . i . spec)
          )
      >>- (const . removeAndSync r $ pure x)

-- | Delete an 'Action' by a given name. Removes all related Schedules too.
--
-- @since 0.1.0
deleteAction
  :: MonadSeer m
  => String                  -- ^ The name of the Action
  -> m (Either Error String) -- ^ The result
deleteAction s = deleteEntity loadActions'
  $ filterAndDeleteByName s A.name S.actionID "Action" removeActions'

-- | Delete a 'Resource' by a given name. Removes all related Schedules too.
--
-- @since 0.1.0
deleteResource
  :: MonadSeer m
  => String                  -- ^ The name of the Resource
  -> m (Either Error String) -- ^ The result
deleteResource s = deleteEntity loadResources'
  $ filterAndDeleteByName s R.name S.resourceID "Resource" removeResources'
