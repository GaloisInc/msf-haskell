-- |Various functions for controlling and interacting with running MSF
-- jobs.
module MSF.Job
  ( module Types.Job
  , job_list
  , job_info
  , job_stop
  , job_list_name_substring
  , job_list_name_substring2
  , job_extended_list
  ) where

import MSF.Monad
import Types.Job
import qualified RPC.Job as RPC

import qualified Data.Map as Map
import Data.List (isInfixOf)

-- | Get a list of the running jobs.
job_list :: (SilentCxt s) => MSF s JobMap
job_list = prim RPC.job_list

-- | Get more information abot jobs.
job_info :: (SilentCxt s) => JobId -> MSF s JobInfo
job_info job = prim $ \ addr auth -> do
  RPC.job_info addr auth job

-- | Stop a given job. Quiet action. Can presumably impact target.
job_stop :: (QuietCxt s) => JobId -> MSF s ()
job_stop job = prim $ \ addr auth -> do
  RPC.job_stop addr auth job

-----------------------------------------------------------------------------

-- |Search for the jobs matching the given substring and return
-- extended information.
job_list_name_substring :: (SilentCxt s) => String -> MSF s [JobInfo]
job_list_name_substring name = do
  jobs <- job_extended_list
  return [j | j <- jobs, name `isInfixOf` (RPC.getJobName (RPC.jobName j))]

-- |Search for the jobs matching the given substring and return
-- the matching job names.
job_list_name_substring2 :: (SilentCxt s) => String -> MSF s [String]
job_list_name_substring2 name  = do
  jobs <- job_list
  return (filter (isInfixOf name) (Map.elems jobs))

-- |Get the JobInfo for all running jobs.
job_extended_list :: (SilentCxt s) => MSF s [JobInfo]
job_extended_list = do
  jobs <- job_list
  jobInfo <- mapM job_info (Map.keys jobs)
  return jobInfo

