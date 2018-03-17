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
  , editAction
  , editResource
  , editSchedule
  , editStorage
  , getActions
  , getConfig
  , getResources
  , getSchedules
  , getStorages
  , setDefaultStorage
  , version
  ) where

import           Control.Lens           (over
                                        ,view
                                        ,(^.)
                                        ,(%~))
import           Control.Monad          (mapM
                                        ,sequence
                                        ,(>=>))
import           Data.Bifunctor         (bimap
                                        ,second)
import           Data.Either            (isLeft)
import           Data.Functor.Const     (Const)
import           Data.List              (isInfixOf
                                        ,sort
                                        ,sortBy
                                        ,transpose)
import qualified Data.Map.Strict as M   (empty
                                        ,insertWith)
import           Data.Time.Calendar     (fromGregorian)
import           Data.Time.Clock        (UTCTime(UTCTime)
                                        ,addUTCTime
                                        ,getCurrentTime)
import           Data.Time.LocalTime    (TimeZone
                                        ,getCurrentTimeZone
                                        ,timeZoneMinutes)
import           Data.UUID              (UUID)
import qualified Seer.Action as A       (Action
                                        ,description
                                        ,duration
                                        ,name
                                        ,new)
import qualified Seer.Config as C       (Config
                                        ,new
                                        ,storage)
import           Seer.DateParser        (parseDate)
import           Seer.Manifest          (Manifest
                                        ,ResourceKind(Action
                                                     ,Resource)
                                        ,ToList (headers
                                                ,toList)
                                        ,creationTimestamp
                                        ,metadata
                                        ,spec
                                        ,uid)
import qualified Seer.Resource as R     (Resource
                                        ,availabilities
                                        ,description
                                        ,name
                                        ,new)
import qualified Seer.Schedule as S     (Schedule
                                        ,ScheduleSpec
                                        ,actionID
                                        ,resourceID
                                        ,start
                                        ,new)
import           Seer.Storage           (Storage
                                        ,configExist
                                        ,editName
                                        ,editRemote
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
                                        ,TimeSpanString
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
                                        ,parseDuration
                                        ,utcToLocal
                                        ,weekAvailableFromTo
                                        ,weekNotAvailable)
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
  configExist' :: m (Either IOError Bool)
  editStorageName' ::  Storage -> Storage -> m (Either IOError ())
  editStorageRemote' ::  Storage -> String -> m (Either IOError ())
  getCurrentTime' :: m UTCTime
  getLine' :: m String
  getCurrentTimeZone' :: m TimeZone
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
  configExist' = configExist
  editStorageName' = editName
  editStorageRemote' = editRemote
  getCurrentTime' = getCurrentTime
  getLine' = getLine
  getCurrentTimeZone' = getCurrentTimeZone
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
leftError = le . ioeGetErrorString

-- | Return a Left error
--
-- @since 0.1.0
le :: MonadSeer m => String -> m (Either Error b)
le = return . Left

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
maybeError e = maybe $ le e

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

-- | The standard message when nothing was done
--
-- @since 0.1.0
nd :: String
nd = "✗ Nothing done"

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
  => String                  -- ^ The name of the Storage to be set
  -> m (Either Error String) -- ^ The result
setDefaultStorage n = storageExist' n >>| f
 where
  f True =
    configExist'
      >>| ( \e -> if e
            then
              loadConfig'
                >>| ( bimapIOErrorDone . saveConfig' . over (spec . C.storage)
                                                            (const n)
                    )
            else bimapIOErrorDone $ newConfig' n >>= saveConfig'
          )
  f _ = le "Storage does not exist"

-- | Get a default Storage. Fails if an IO error occurs.
--
-- @since 0.1.0
getDefaultStorage :: MonadSeer m => m (Either IOError String)
getDefaultStorage = second (view $ spec . C.storage) <$> loadConfig'

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
    utcToLocal' (e ^. metadata . creationTimestamp)
      >>= (\l -> return $ show i : toList e ++ [l])
  h = ["#"] ++ headers (head a) ++ ["CREATED"]
  sortCreated x y =
    if x ^. metadata . creationTimestamp > y ^. metadata . creationTimestamp
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

-- | Retrieve all entities from the default Storage
--
-- @since 0.1.0
getAllEntities
  :: MonadSeer m
  => ([R.Resource] -> [A.Action] -> [S.Schedule] -> m (Either Error b))
  -> m (Either Error b)
getAllEntities f =
  getDefaultStorage
    >>| ( \d -> do
          sx <- loadSchedules' d
          rx <- loadResources' d
          ax <- loadActions' d
          case (sx, rx, ax) of
            (Left e , _      , _      ) -> leftError e
            (_      , Left e , _      ) -> leftError e
            (_      , _      , Left e ) -> leftError e
            (Right s, Right r, Right a) -> f r a s
        )

-- | Validate the given entity constellation
--
-- @since 0.1.0
validateEnditites
  :: [R.Resource] -- ^ The input Resources
  -> [A.Action]   -- ^ The input Actions
  -> [S.Schedule] -- ^ The input Schedules
  -> Bool         -- ^ The result
validateEnditites rx ax sx =
  let ts = (\s -> (s, fe S.actionID s ax, fe S.resourceID s rx)) <$> sx
  in  not (wrongCnt ts) && (and . foldr validate [] $ foldr group M.empty ts)
 where
  fe e s = filter (\x -> s ^. spec . e == uuid x)
  wrongCnt = any $ \(_, a, r) -> cl a && cl r
  cl x = length x /= 1
  group (s, a:_, r:_) c =
    maybe c (\e -> M.insertWith (++) r [(start s, e)] c) $ end (start s) r a
  group _ c = c
  validate r a = timesValid (sort r) : a
  nilTime = UTCTime (fromGregorian 0 0 0) 0
  timesValid =
    fst <$> foldl (\(_, p) (s, e) -> validateTime p s e) (True, nilTime)
  validateTime p s e | s < e && s >= p = (True, e)
                     | otherwise       = (False, e)

-- | Retrieve the UUID from en Entity
--
-- @since 0.1.0
uuid :: Manifest s -> UUID
uuid x = x ^. metadata . uid

-- | List all available Schedules for the default Storage. Evaluates to either
-- a fully formatted table (Right) or an error message (Left). The second
-- argument specifies if all schedules or just the future ones should be shown.
--
-- @since 0.1.0
getSchedules
  :: MonadSeer m
  => Bool                    -- ^ True if also past schedules should be shown
  -> m (Either Error String) -- ^ The result
getSchedules showAll = getAllEntities
  ( \r a s ->
    getCurrentTime'
      >>= (   rows r a s
          >=> ( \k -> return . Right $ if null k
                                       then
                                         nf
                                       else
                                         tablify $ header : k
              )
          )
  )
 where
  header = ["#", "FROM", "TO", "Σ", "RESOURCE", "ACTION", "CREATED"]
  rows r a s t = mapM
    ( \(i, v) -> do
      cr <- utcToLocal' $ v ^. metadata . creationTimestamp
      let fr = dateTimeFormat $ start v
      let ru = v ^. spec . S.resourceID
      let au = v ^. spec . S.actionID
      let l  = [show i, fr]
      case (fuid ru r, fuid au a) of
        (x:_, y:_) -> return $ l ++ [e v x y, adura y, rname x, aname y, cr]
        (_  , y:_) -> return $ l ++ [n, adura y, n, aname y, cr]
        (x:_, _  ) -> return $ l ++ [n, n, rname x, n, cr]
        (_  , _  ) -> return $ l ++ [n, n, n, n, cr]
    )
    (zip [1 :: Int ..] . filter (past t) $ sortBy sortStart s)
  past t x = showAll || start x >= t
  fuid a = filter ((==) a . uuid)
  e v x y = maybe "" dateTimeFormat $ end (start v) x y
  n = "✗ not available"
  rname x = x ^. spec . R.name
  aname x = x ^. spec . A.name
  adura x = show $ x ^. spec . A.duration

-- | Sort schedules by starting date
--
-- @since 0.1.0
sortStart :: S.Schedule -> S.Schedule -> Ordering
sortStart x y = if start x > start y then GT else LT

-- | Retrieve the start of a Schedule
--
-- @since 0.1.0
start :: S.Schedule -> UTCTime
start x = x ^. spec . S.start

-- | Evaluate the end time for a given start, 'Resource' and 'Action'
--
-- @since 0.1.0
end :: UTCTime -> R.Resource -> A.Action -> Maybe UTCTime
end x y z =
  evaluateEnd x (y ^. spec . R.availabilities) (z ^. spec . A.duration)

-- | Create a new Storage and evaluate to an Error if anything went wrong.
--
-- @since 0.1.0
createStorage
  :: MonadSeer m
  => String                  -- ^ The name of the Storage
  -> Maybe String            -- ^ The remote of the Storage
  -> m (Either Error String) -- ^ The result
createStorage n r = newStorage' n r >>| const (setDefaultStorage n)

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
  -> Maybe String            -- ^ The description of the Action
  -> String                  -- ^ The duration as a String
  -> m (Either Error String) -- ^ The result
createAction n d r = newAction' n d r
  >>= maybeError "Unable to create Action" (saveAndSync saveActions')

-- | Create a new 'Resource' for a given name, description and available times.
-- The available times in the list needs to be in the order from Monday to
-- Sunday, whereas empty Strings are allowed.
--
-- @since 0.1.0
createResource
  :: MonadSeer m
  => String         -- ^ The name of the Resource
  -> Maybe String   -- ^ The description of the Resource
  -> ( Maybe String
     , Maybe String
     , Maybe String
     , Maybe String
     , Maybe String
     , Maybe String
     , Maybe String
     ) -- ^ The time span Strings
  -> m (Either Error String) -- ^ The result
createResource _ _ (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
  = le "A resource without any availability can't do anything."
createResource n d a =
  maybeError "Unable to parse time spans"
             (newResource' n d >=> saveAndSync saveResources')
    $ toAvailability a

-- | Convert optional arguments to weekly availability
--
-- @since 0.1.0
toAvailability
  :: ( Maybe TimeSpanString
     , Maybe TimeSpanString
     , Maybe TimeSpanString
     , Maybe TimeSpanString
     , Maybe TimeSpanString
     , Maybe TimeSpanString
     , Maybe TimeSpanString
     )
  -> Maybe Availabilities
toAvailability (m, t, w, h, f, s, u) =
  weekAvailableFromTo
    $  i Mon m
    ++ i Tue t
    ++ i Wed w
    ++ i Thu h
    ++ i Fri f
    ++ i Sat s
    ++ i Sun u
 where
  i _ Nothing  = []
  i x (Just y) = pure $ (,) x y

-- | Filter an entity by a given filter function
--
-- @since 0.1.0
filterEntity
  :: String       -- ^ The filter String
  -> ((String -> Const String String) -> a -> Const String a) -- ^ The filter applicator
  -> [Manifest a] -- ^ The list to be filtered
  -> [Manifest a] -- ^ The result
filterEntity s n = filter (\x -> s `isInfixOf` (x ^. spec . n))

-- | Convert the local time to the same time in UTC format
--
-- @since 0.1.0
parseDateLocal
  :: MonadSeer m
  => String                           -- ^ The string to be parsed
  -> (UTCTime -> m (Either String b)) -- ^ The succeeding function
  -> m (Either String b)              -- ^ The result
parseDateLocal s f = do
  time     <- getCurrentTime'
  timeZone <- getCurrentTimeZone'
  let t = addUTCTime (fromIntegral $ timeZoneMinutes timeZone * 60) time
  case parseDate t s of
    Left  e -> le $ printf "Unable to parse date '%s'" e
    Right r -> f r

-- | Create a new 'Schedule' for two given date strings (from/to), an
-- 'Resource' and 'Action' name.
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
    >>| ( \d -> do
          resources <- loadResources' d
          actions   <- loadActions' d
          parseDateLocal
            f
            ( \parsedDate -> case (resources, actions) of
              (Left e, _     ) -> leftError e
              (_     , Left e) -> leftError e
              (Right rx, Right ax) ->
                chooseActionAndResource r a rx ax (buildSchedule d parsedDate)
            )
        )
 where
  isInRange (Just x) y (Just z) | x >= y && x <= z = True
                                | otherwise        = False
  isInRange _ _ _ = False
  buildSchedule d p re ac =
    case evaluateStart p (re ^. spec . R.availabilities) of
      Nothing -> le "Unable calculate start date"
      Just from ->
        let e = end from re ac
        in  logTime from e
            >>  loadSchedules' d
            >>| ( \s ->
                  if not
                     .   or
                     $   (\x -> isInRange e (start x) (end (start x) re ac))
                     <$> filter (\x -> uuid re == x ^. spec . S.resourceID) s
                  then
                    newSchedule' from (uuid re) (uuid ac)
                      >>= saveAndSync saveSchedules'
                  else
                    le "The Action is not schedulable at this time"
                )
  logTime x (Just y) = log' $ printf "Using calculated date range: %s - %s"
                                     (dateTimeFormat x)
                                     (dateTimeFormat y)
  logTime x _ =
    log' . printf "Unable to calculate end time for start: %s" $ dateTimeFormat
      x

-- | Higher order 'Action' and 'Resource' chooser by names.
--
-- @since 0.1.0
chooseActionAndResource
  :: (MonadSeer m)
  => String
  -> String
  -> [R.Resource]
  -> [A.Action]
  -> (R.Resource -> A.Action -> m (Either String b))
  -> m (Either String b)
chooseActionAndResource r a rx ax f = do
  rres <- chooseResource r rx
  ares <- chooseAction a ax
  case (rres, ares) of
    (Right ae, Right re) -> f ae re
    (Left  e , _       ) -> le e
    (_       , Left e  ) -> le e

-- | Higher order 'Action' chooser by name.
--
-- @since 0.1.0
chooseAction :: MonadSeer m => String -> [A.Action] -> m (Either Error A.Action)
chooseAction = chooseEntity A.name (show Action)

-- | Higher order 'Resource' chooser by name.
--
-- @since 0.1.0
chooseResource
  :: MonadSeer m => String -> [R.Resource] -> m (Either Error R.Resource)
chooseResource = chooseEntity R.name (show Resource)

-- | Higher order entity chooser.
--
-- @since 0.1.0
chooseEntity
  :: (ToList a, MonadSeer m)
  => ((String -> Const String String) -> a -> Const String a)
  -> String
  -> String
  -> [Manifest a]
  -> m (Either Error (Manifest a))
chooseEntity e es a ax = case filterEntity a e ax of
  []  -> leftNotFound es a
  amx -> do
    ca <- chooseOneOf es amx
    case ca of
      Nothing -> le $ printf "Wrong input for the %s" es
      Just ae -> return $ Right ae

-- | Return a Left not found error
--
-- @since 0.1.0
leftNotFound :: MonadSeer m => String -> String -> m (Either Error b)
leftNotFound x y = le $ printf "No %s found for '%s'" x y

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

-- | Delete a 'Schedule' by a given number.
--
-- @since 0.1.0
deleteSchedule
  :: MonadSeer m
  => String                   -- ^ The number of the Schedule
  -> m (Either Error String)  -- ^ The result
deleteSchedule s = deleteEntity loadSchedules' filterAndDelete
 where
  filterAndDelete _ sx =
    filterSchedule s sx (removeAndSync removeSchedules' . pure)

-- | A generic 'Schedule' filtering by start UTCTime
--
-- @since 0.1.0
filterSchedule
  :: MonadSeer m
  => String
  -> [S.Schedule]
  -> (S.Schedule -> m (Either String b))
  -> m (Either Error b)
filterSchedule s sx f =
  let sorted = sortBy sortStart sx
  in  case reads s :: [(Int, String)] of
        (i, _):_ | itemOf (i - 1) -> f $ sorted !! (i - 1)
                 | otherwise      -> le "No valid Schedule number"
        _ -> le $ printf "Unable to parse '%s' as integer." s
 where
  itemOf i | i >= length sx || i < 0 = False
           | otherwise               = True

-- | A generic entity deletion function for filtering by their name
--
-- @since 0.1.0
filterAndDeleteByName
  :: (Applicative f, MonadSeer m, ToList a)
  => String                                                   -- ^ The name to be removed
  -> ((String -> Const String String) -> a -> Const String a) -- ^ The field to be filtered
  -> (  (UUID -> Const UUID UUID)
     -> S.ScheduleSpec
     -> Const UUID S.ScheduleSpec
     )                                                        -- ^ The uuid retrieval function
  -> String                                                   -- ^ The name of the entity type
  -> (String -> f (Manifest a) -> m (Either IOError b))       -- ^ The entity removal function
  -> Storage                                                  -- ^ The storage to be changed
  -> [Manifest a]                                             -- ^ The entity to be filtered 
  -> m (Either Error String)                                  -- ^ The result
filterAndDeleteByName s f i n r d xs = case filterEntity s f xs of
  [] -> le $ printf "No %s found for name '%s'" n s
  es -> chooseOneOf n es >>= maybeError "Got wrong input" rmAndSync
 where
  rmAndSync x =
    loadSchedules' d
      >>| ( removeAndSync removeSchedules'
          . filter (\y -> x ^. metadata . uid == y ^. spec . i)
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
  $ filterAndDeleteByName s A.name S.actionID (show Action) removeActions'

-- | Delete a 'Resource' by a given name. Removes all related Schedules too.
--
-- @since 0.1.0
deleteResource
  :: MonadSeer m
  => String                  -- ^ The name of the Resource
  -> m (Either Error String) -- ^ The result
deleteResource s = deleteEntity loadResources' $ filterAndDeleteByName
  s
  R.name
  S.resourceID
  (show Resource)
  removeResources'

-- | Edit a 'Storage' by a given name. Change the actual name or remote
-- location of the Storage. For the removal of a remote location of the
-- Storage simply pass a 'Just ""' in.
--
-- @since 0.1.0
editStorage
  :: MonadSeer m
  => String                  -- ^ The current name of the Storage
  -> Maybe String            -- ^ The new name of the Storage
  -> Maybe String            -- ^ The new remote of the Storage
  -> m (Either Error String) -- ^ The result
editStorage _ Nothing  Nothing  = return $ Right nd
editStorage n Nothing  (Just r) = bimapIOErrorDone $ editStorageRemote' n r
editStorage n (Just m) Nothing  = bimapIOErrorDone $ editStorageName' n m
editStorage n (Just m) (Just r) =
  bimapIOErrorDone (editStorageName' n m)
    >>= ( \res -> if isLeft res
          then return res
          else bimapIOErrorDone $ editStorageRemote' m r
        )

-- | Edit an 'Action' by a given name. Change the actual description or duration
-- of the 'Action'.
--
-- @since 0.1.0
editAction
  :: MonadSeer m
  => String                  -- ^ The name of the Action
  -> Maybe String            -- ^ The new name of the Action
  -> Maybe String            -- ^ The new description of the Action
  -> Maybe String            -- ^ The new duration of the Action
  -> m (Either Error String) -- ^ The result
editAction _ Nothing Nothing Nothing = return $ Right nd
editAction n m       d       u       = getAllEntities
  ( \rx ax sx -> case filterEntity n A.name ax of
    [] -> rnf
    af -> chooseOneOf (show Action) af >>= maybe
      rnf
      ( \a -> either
        le
        ( \x -> if validateEnditites rx (newActions ax x) sx
          then saveAndSync saveActions' x
          else le "Unable to change duration"
        )
        (chDur u . chName m $ chDesc d a)
      )
  )
 where
  rnf = return $ Right nf
  chName (Just x) a = spec . A.name %~ const x $ a
  chName _        a = a
  chDesc (Just x) a = spec . A.description %~ const (Just x) $ a
  chDesc _        a = a
  chDur (Just x) a =
    maybe (Left "Unable to parse duration")
          (\e -> Right $ spec . A.duration %~ const e $ a)
      $ parseDuration x
  chDur _ a = Right a
  newActions ax x = (\y -> if uuid x == uuid y then x else y) <$> ax

-- | Edit a 'Resource' by the given name.
--
-- @since 0.1.0
editResource
  :: MonadSeer m
  => String         -- ^ The name of the Resource
  -> Maybe String   -- ^ The new name of the Resource
  -> Maybe String   -- ^ The description of the Resource
  -> ( Maybe String
     , Maybe String
     , Maybe String
     , Maybe String
     , Maybe String
     , Maybe String
     , Maybe String
     ) -- ^ The time span Strings
  -> m (Either Error String) -- ^ The result
editResource _ Nothing Nothing (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
  = return $ Right nd
editResource n m d a = getAllEntities
  ( \rx ax sx -> case filterEntity n R.name rx of
    [] -> rnf
    rf -> chooseOneOf (show Resource) rf >>= maybe
      rnf
      ( \r -> do
        let nr = chAv (toAvailability a) . chName m $ chDesc d r
        case sequence (adaptSchedules nr <$> sx) of
          Nothing -> le "Unable to rearrange schedules"
          Just ns ->
            if validateEnditites (newResources nr rx) ax ns
            then
              getDefaultStorage
                >>| ( \x -> saveSchedules' x ns
                      >>| return (saveAndSync saveResources' nr)
                    )
            else
              le "Unable to change availabilities"
      )
  )
 where
  adaptSchedules r x =
    let maybeStart =
          evaluateStart (x ^. spec . S.start) (r ^. spec . R.availabilities)
    in  case maybeStart of
          Nothing -> Nothing
          Just s  -> if x ^. spec . S.resourceID == uuid r
            then Just (spec . S.start %~ const s $ x)
            else Just x
  chName (Just x) r = spec . R.name %~ const x $ r
  chName _        r = r
  chDesc (Just x) r = spec . R.description %~ const (Just x) $ r
  chDesc _        r = r
  chAv (Just x) r | x == weekNotAvailable = r
                  | otherwise = spec . R.availabilities %~ const x $ r
  chAv _ r = r
  newResources x rx = (\y -> if uuid x == uuid y then x else y) <$> rx
  rnf = return $ Right nf

-- | Edit a 'Schedule' by a given number. Change the actual start, 'Action' or
-- 'Resource' of the 'Schedule'.
--
-- @since 0.1.0
editSchedule
  :: MonadSeer m
  => String       -- ^ The number of the Schedule
  -> Maybe String -- ^ The new start date
  -> Maybe String -- ^ The new 'Resource' name
  -> Maybe String -- ^ The new 'Action' name
  -> m (Either Error String)
editSchedule _ Nothing Nothing Nothing = return $ Right nd
editSchedule s n       r       a       = getAllEntities
  ( \rx ax sx -> filterSchedule
    s
    sx
    ( \x -> do
      newStart <- parseNewStart n
      action   <- chooseActionMaybe a ax
      resource <- chooseResourceMaybe r rx
      case (newStart, action, resource) of
        (Left e  , _       , _       ) -> le e
        (_       , Left e  , _       ) -> le e
        (_       , _       , Left e  ) -> le e
        (Right ns, Right ac, Right re) -> do
          let cs = evaluateStartMaybe ns $ filterResource x rx
          let sc = chResource re . chAction ac $ chStart cs x
          if validateEnditites rx ax (newSchedules sx sc)
            then saveAndSync saveSchedules' sc
            else le "Unable to change Schedule"
    )
  )
 where
  parseNewStart (Just x) = parseDateLocal x (return . Right . Just)
  parseNewStart _        = return $ Right Nothing
  chooseActionMaybe (Just x) ax = chooseAction x ax >>- (return . Right . Just)
  chooseActionMaybe _        _  = return $ Right Nothing
  chooseResourceMaybe (Just x) ax =
    chooseResource x ax >>- (return . Right . Just)
  chooseResourceMaybe _ _ = return $ Right Nothing
  evaluateStartMaybe (Just x) (Just re) =
    evaluateStart x (re ^. spec . R.availabilities)
  evaluateStartMaybe _ _ = Nothing
  filterResource x rx =
    case filter (\y -> uuid y == x ^. spec . S.resourceID) rx of
      [y] -> Just y
      _   -> Nothing
  chStart (Just t) x = spec . S.start %~ const t $ x
  chStart _        x = x
  chAction (Just u) x = spec . S.actionID %~ const (uuid u) $ x
  chAction _        x = x
  chResource (Just u) x = spec . S.resourceID %~ const (uuid u) $ x
  chResource _        x = x
  newSchedules sx x = (\y -> if uuid x == uuid y then x else y) <$> sx
