-- |The MSF server can interact with a database where it stores things
-- like the discovered hosts, services, credentials, etc.
module RPC.DB
  ( module Types.DB
  , db_hosts
  , db_services
  , db_creds
  , db_loots
  , db_vulns
  , db_report_host
  , db_report_service
  , db_report_cred
  , db_report_loot
  , db_report_vuln
  , db_status
  , db_get_auth_info
  ) where

import Types.DB

import MSF.Host (Con,Server)

db_hosts :: Con Server -> Token -> IO [HostInfo]
db_hosts addr auth = field "db.hosts" "hosts" =<< send_request "db.hosts" addr
  [ toObject auth
  , ObjectArray []
  ]

db_report_host :: Con Server -> Token -> NewHost -> IO Result
db_report_host addr auth hst = send_request "db.report_host" addr
  [ toObject auth
  , toObject hst
  ]

db_services :: Con Server -> Token -> IO [Service]
db_services addr auth = field "db.services" "services" =<< send_request "db.services" addr
  [ toObject auth
  , ObjectArray []
  ]

db_report_service :: Con Server -> Token -> NewService -> IO Result
db_report_service addr auth srv = send_request "db.report_service" addr
  [ toObject auth
  , toObject srv
  ]

db_creds :: Con Server -> Token -> IO [Cred]
db_creds addr auth = field "db.creds" "creds" =<< send_request "db.creds" addr
  [ toObject auth
  , ObjectArray []
  ]

db_report_cred :: Con Server -> Token -> NewCred -> IO Result
db_report_cred addr auth crd = send_request "db.report_cred" addr
  [ toObject auth
  , toObject crd
  ]

db_loots :: Con Server -> Token -> IO [Loot]
db_loots addr auth = field "db.loots" "loots" =<< send_request "db.loots" addr
  [ toObject auth
  , ObjectArray []
  ]

db_report_loot :: Con Server -> Token -> NewLoot -> IO Result
db_report_loot addr auth loot = send_request "db.report_loot" addr
  [ toObject auth
  , toObject loot
  ]

db_vulns :: Con Server -> Token -> IO Object
db_vulns addr auth = field "db.vulns" "vulns" =<< send_request "db.vulns" addr
  [ toObject auth
  , ObjectArray []
  ]

db_report_vuln :: Con Server -> Token -> NewVuln -> IO Result
db_report_vuln addr auth vln = send_request "db.report_vuln" addr
  [ toObject auth
  , toObject vln
  ]

db_status :: Con Server -> Token -> IO Object
db_status addr auth = send_request "db.status" addr
  [ toObject auth
  ]

db_get_auth_info :: Con Server -> Token -> IO Object
db_get_auth_info addr auth = send_request "db.get_auth_info" addr
  [ toObject auth
  , ObjectArray []
  ]

---- Other RPCs pulled from msf3/lib/msf/core/rpc/v10/rpc_db.rb
-- db.add_workspace(wspace)
-- db.clients(xopts)
-- db.connect(xopts)
-- db.current_workspace
-- db.del_client(xopts)
-- db.del_host(xopts)
-- db.del_note(xopts)
-- db.del_service(xopts)
-- db.del_vuln(xopts)
-- db.del_workspace(wspace)
-- db.disconnect
-- db.driver(xopts)
-- db.events(xopts)
-- db.get_client(xopts)
-- db.get_host(xopts)
-- db.get_note(xopts)
-- db.get_ref(name)
-- db.get_service(xopts)
-- db.get_vuln(xopts)
-- db.get_workspace(wspace)
-- db.import_data(xopts)
-- db.notes(xopts)
-- db.report_auth_info(xopts)
-- db.report_client(xopts)
-- db.report_cred(xopts)
-- db.report_event(xopts)
-- db.report_host(xopts)
-- db.report_loot(xopts)
-- db.report_note(xopts)
-- db.report_service(xopts)
-- db.report_vuln(xopts)
-- db.set_workspace(wspace)
-- db.workspaces

