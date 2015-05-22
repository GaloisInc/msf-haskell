-- |Helpful high-level commands for resetting the MSF server to
-- cleaner states.
module MSF.Commands where

import MSF.Console
import MSF.Monad

-- |Deletes all loot as when running /loot -d/ in msfconsole.
delete_loots :: (QuietCxt s) => MSF s ()
delete_loots = console_write "loot -d\n"

-- |Stop all the sessions as when runnning /sessions -K/ in msfconsole.
stop_sessions :: (QuietCxt s) => MSF s ()
stop_sessions = console_write "sessions -K\n"

-- |Removes known hosts as when running /hosts -d/ in msfconsole.
remove_hosts :: (QuietCxt s) => MSF s ()
remove_hosts = console_write "hosts -d\n"

-- |Stops running services as when running /services -d/ in msfconsole.
stop_services :: (QuietCxt s) => MSF s ()
stop_services = console_write "services -d\n"

-- |Kills all running jobs as when running /jobs -K/ in msfconsole.
kill_jobs :: (QuietCxt s) => MSF s ()
kill_jobs = console_write "jobs -K\n"
