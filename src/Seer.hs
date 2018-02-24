-- | This module everything related to the main library interface
--
-- @since 0.1.0

module Seer
  ( Error
  , MonadSeer
  , createAction
  , createResource
  , createStorage
  , createSchedule
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
import           Data.Time.Clock        (UTCTime)
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
  getLine' = getLine
  list' = list
  loadActions' = loadActions
  loadConfig' = loadConfig
  loadResources' = loadResources
  loadSchedules' = loadSchedules
  log' = putStrLn . (++) "- "
  newAction' = A.new
  newConfig' = C.new
  newResource' = R.new
  newSchedule' = S.new
  newStorage' = new
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
tablify r = render . hsep 2 left $ vcat left . map text <$> transpose r

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
-- a fully formatted table (Right) or an error message (Left).
--
-- @since 0.1.0
getSchedules :: MonadSeer m => m (Either Error String)
getSchedules =
  getDefaultStorage
    >>| ( \d -> do
          sx <- loadSchedules' d
          rx <- loadResources' d
          ax <- loadActions' d
          case (sx, rx, ax) of
            (Left e , _      , _      ) -> leftError e
            (_      , Left e , _      ) -> leftError e
            (_      , _      , Left e ) -> leftError e
            (Right s, Right r, Right a) -> if null s
              then return $ Right nf
              else (Right . tablify . (:) header) <$> rows r a s
        )
 where
  end v x y = maybe "" dateTimeFormat $ evaluateEnd
    (S.start $ spec v)
    (R.availabilities $ spec x)
    (A.duration $ spec y)
  n     = "✗ not available"
  rname = R.name . spec
  aname = A.name . spec
  fuid a = filter (\x -> uid (metadata x) == a)
  sortFrom x y = if S.start (spec x) > S.start (spec y) then GT else LT
  header = ["FROM", "TO", "RESOURCE", "ACTION", "CREATED"]
  rows r a s = mapM
    ( \v -> do
      cr <- utcToLocal' (creationTimestamp $ metadata v)
      let fr = dateTimeFormat . S.start $ spec v
      let ru = S.resourceID $ spec v
      let au = S.actionID $ spec v
      case (fuid ru r, fuid au a) of
        (x:_, y:_) -> return [fr, end v x y, rname x, aname y, cr]
        (_  , y:_) -> return [fr, n, n, aname y, cr]
        (x:_, _  ) -> return [fr, n, rname x, n, cr]
        (_  , _  ) -> return [fr, n, n, n, cr]
    )
    (sortBy sortFrom s)

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
defaultSync
  :: (Applicative f, MonadSeer m)
  => (String -> f a -> m (Either IOError b)) -- ^ The entity save function
  -> a                                       -- ^ The entity to be saved
  -> m (Either Error String)                 -- ^ The result
defaultSync f x =
  getDefaultStorage
    >>| (\s -> f s (pure x) >>| (const . bimapIOErrorDone $ save' s))

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
  >>= maybeError "Unable to create Action" (defaultSync saveActions')

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
             (newResource' n (maybeString d) >=> defaultSync saveResources')
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
                    ( filter (nameContains r R.name) allResources
                    , filter (nameContains a A.name) allActions
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
  as   = "Action"
  rs   = "Resource"
  uuid = uid . metadata
  isInRange (Just x) y (Just z) | x >= y && x <= z = True
                                | otherwise        = False
  isInRange _ _ _ = False
  startDate x = S.start $ spec x
  endDate x y z =
    evaluateEnd x (R.availabilities $ spec y) (A.duration $ spec z)
  nameContains x y = isInfixOf x . y . spec
  leftParseError x = le $ printf "Unable to parse date '%s'" x
  leftNotFound x y = le $ printf "No %s found for '%s'" x y
  le x = return $ Left x
  buildSchedule d p re ac =
    case evaluateStart p (R.availabilities $ spec re) of
      Nothing -> le "Unable calculate start date"
      Just from ->
        log' (printf "Using calculated start time at %s" $ dateTimeFormat from)
          >>  loadSchedules' d
          >>| ( \s ->
                if not
                     .   or
                     $   ( \x -> isInRange (endDate from re ac)
                                           (startDate x)
                                           (endDate (startDate x) re ac)
                         )
                     <$> filter (\x -> uuid re == S.resourceID (spec x)) s
                  then newSchedule' from (uuid re) (uuid ac)
                    >>= defaultSync saveSchedules'
                  else le "The action is not schedulable at this date"
              )

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
    ((i, _):_) -> return . itemOf $ i - 1
    _          -> return Nothing
 where
  itemOf i | i >= l || i < 0 = Nothing
           | otherwise       = Just (xs !! i)
  l = length xs
