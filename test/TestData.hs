-- | Test relevant static data
--
-- @since 0.1.0

module TestData
  ( testAction
  , testActionRealDuration
  , testConfig
  , testResource
  , testSchedule
  , testScheduleAfter
  , testMetadata
  , testTime
  , testError
  ) where

import           Data.Time.Calendar       (fromGregorian)
import           Data.Time.Clock          (UTCTime (UTCTime
                                                   ,utctDay
                                                   ,utctDayTime))
import           Data.UUID.Types.Internal (buildFromWords)
import           Data.Word                (Word8)
import qualified Seer.Action as A         (Action
                                          ,newSpec)
import qualified Seer.Config as C         (Config
                                          ,newSpec)
import           Seer.Manifest            (ApiVersion (V1)
                                          ,newManifest
                                          ,Metadata
                                          ,newMetadata
                                          ,ResourceKind (Action
                                                        ,Config
                                                        ,Resource
                                                        ,Schedule))
import qualified Seer.Resource as R       (Resource
                                          ,newSpec)
import qualified Seer.Schedule as S       (Schedule
                                          ,newSpec)
import           Seer.Time                (weekAvailable
                                          ,Duration (Duration))

testAction :: A.Action
testAction = newManifest V1
                         Action
                         (testMetadata 1)
                         (A.newSpec "action" Nothing (Duration 5))

testConfig :: C.Config
testConfig = newManifest V1 Config (testMetadata 2) (C.newSpec "storage")

testResource :: R.Resource
testResource = newManifest V1
                           Resource
                           (testMetadata 3)
                           (R.newSpec "resource" Nothing weekAvailable)

testSchedule :: S.Schedule
testSchedule = newManifest
  V1
  Schedule
  (testMetadata 4)
  (S.newSpec testTime (buildFromWords 3 0 0 0 0) (buildFromWords 1 0 0 0 0))

testScheduleAfter :: S.Schedule
testScheduleAfter = newManifest
  V1
  Schedule
  (testMetadata 4)
  ( S.newSpec testTimeAfter
              (buildFromWords 3 0 0 0 0)
              (buildFromWords 1 0 0 0 0)
  )

testMetadata :: Word8 -> Metadata
testMetadata x = newMetadata testTime (buildFromWords x 0 0 0 0)

testTime :: UTCTime
testTime = UTCTime {utctDay = fromGregorian 0 0 0, utctDayTime = 0}

testTimeAfter :: UTCTime
testTimeAfter = UTCTime {utctDay = fromGregorian 0 0 0, utctDayTime = 5 * 60}

testError :: Either IOError b
testError = Left $ userError "failure"

testActionRealDuration :: A.Action
testActionRealDuration = newManifest
  V1
  Action
  (testMetadata 5)
  (A.newSpec "action" Nothing (Duration 60))
