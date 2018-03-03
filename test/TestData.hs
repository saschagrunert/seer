-- | Test relevant static data
--
-- @since 0.1.0

module TestData
  ( testAction
  , testActionRealDuration
  , testConfig
  , testResource
  , testSchedule
  , testMetadata
  , testTime
  , testTime2000
  , testError
  ) where

import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock    (UTCTime (UTCTime
                                             ,utctDay
                                             ,utctDayTime))
import           Data.UUID          (nil)
import qualified Seer.Action as A   (Action
                                    ,ActionSpec (ActionSpec
                                                ,description
                                                ,duration
                                                ,name))
import qualified Seer.Config as C   (Config
                                    ,ConfigSpec (ConfigSpec
                                                ,storage))
import           Seer.Manifest      (ApiVersion (V1)
                                    ,Manifest (Manifest
                                              ,apiVersion
                                              ,kind
                                              ,metadata
                                              ,spec)
                                    ,Metadata (Metadata
                                              ,creationTimestamp
                                              ,uid)
                                    ,ResourceKind (Action
                                                  ,Config
                                                  ,Resource
                                                  ,Schedule))
import qualified Seer.Resource as R (Resource
                                    ,ResourceSpec (ResourceSpec
                                                  ,availabilities
                                                  ,description
                                                  ,name))
import qualified Seer.Schedule as S (Schedule
                                    ,ScheduleSpec (ScheduleSpec
                                                  ,actionID
                                                  ,resourceID
                                                  ,start))
import           Seer.Time          (weekAvailable
                                    ,Duration (Duration))

testAction :: A.Action
testAction = Manifest
  { apiVersion = V1
  , kind       = Action
  , metadata   = testMetadata
  , spec       = A.ActionSpec
    { A.name        = "action"
    , A.description = Nothing
    , A.duration    = Duration 0
    }
  }

testConfig :: C.Config
testConfig = Manifest
  { apiVersion = V1
  , kind       = Config
  , metadata   = testMetadata
  , spec       = C.ConfigSpec {C.storage = "storage"}
  }

testResource :: R.Resource
testResource = Manifest
  { apiVersion = V1
  , kind       = Resource
  , metadata   = testMetadata
  , spec       = R.ResourceSpec
    { R.name           = "resource"
    , R.description    = Nothing
    , R.availabilities = weekAvailable
    }
  }

testSchedule :: S.Schedule
testSchedule = Manifest
  { apiVersion = V1
  , kind       = Schedule
  , metadata   = testMetadata
  , spec       = S.ScheduleSpec
    { S.start      = testTime
    , S.resourceID = nil
    , S.actionID   = nil
    }
  }

testMetadata :: Metadata
testMetadata = Metadata {creationTimestamp = testTime, uid = nil}

testTime :: UTCTime
testTime = UTCTime {utctDay = fromGregorian 0 0 0, utctDayTime = 0}

testTime2000 :: UTCTime
testTime2000 = UTCTime {utctDay = fromGregorian 0 0 2000, utctDayTime = 0}

testError :: Either IOError b
testError = Left $ userError "failure"

testActionRealDuration :: A.Action
testActionRealDuration = Manifest
  { apiVersion = V1
  , kind       = Action
  , metadata   = testMetadata
  , spec       = A.ActionSpec
    { A.name        = "action"
    , A.description = Nothing
    , A.duration    = Duration 60
    }
  }
