-- |Various functions for controlling and interacting with running MSF
-- jobs.
module RPC.Job
  ( module Types.Job
  , job_list
  , job_info
  , job_stop
  ) where

import MSF.Host (Con,Server)
import Types.Job

-- | Get a list of the running jobs. Silent operation.
job_list :: Con Server -> Token -> IO JobMap
job_list addr auth = send_request "job.list" addr
  [ toObject auth
  ]

-- | Get more information abot jobs. Silent operation.
job_info :: Con Server -> Token -> JobId -> IO JobInfo
job_info addr auth jobID = send_request "job.info" addr
  [ toObject auth
  , toObject jobID
  ]

-- | Stop a given job. Quiet action. Can presumably impact target.
job_stop :: Con Server -> Token -> JobId -> IO ()
job_stop addr auth jobID = success "job.stop" =<< send_request "job.stop" addr
  [ toObject auth
  , toObject jobID
  ]

