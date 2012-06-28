{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module GameKeeper.Connections (
    Connection
  , connections
  ) where

import GHC.Generics    (Generic)
import Data.Data
import Data.Aeson      (FromJSON, decode')
import Data.Aeson.Types
import GameKeeper.Http (getBody)

--
-- Example Connection JSON:
--
-- {
--     "recv_oct": 439,
--     "recv_cnt": 10,
--     "send_oct": 4387281,
--     "send_cnt": 9280,
--     "send_pend": 0,
--     "state": "running",
--     "channels": 2,
--     "recv_oct_details": {
--         "rate": 0,
--         "interval": 5001203,
--         "last_event": 1340193202054
--     },
--     "send_oct_details": {
--         "rate": 0,
--         "interval": 5001203,
--         "last_event": 1340193202054
--     },
--     "name": "...:50389",
--     "type": "network",
--     "node": "rabbit@...",
--     "address": "10....",
--     "port": 5672,
--     "peer_address": "10....",
--     "peer_port": 50389,
--     "ssl": false,
--     "peer_cert_subject": "",
--     "peer_cert_issuer": "",
--     "peer_cert_validity": "",
--     "auth_mechanism": "AMQPLAIN",
--     "ssl_protocol": "",
--     "ssl_key_exchange": "",
--     "ssl_cipher": "",
--     "ssl_hash": "",
--     "protocol": "AMQP 0-9-1",
--     "user": "...",
--     "vhost": "/",
--     "timeout": 0,
--     "frame_max": 131072,
--     "client_properties": {
--         "version": "0.0.1",
--         "platform": "node-v0.6.17",
--         "product": "node-amqp"
--     }
-- }
--

data TrafficDetails = TrafficDetails
    { last_event :: Integer
    } deriving (Show, Generic, Data, Typeable)

instance FromJSON TrafficDetails

data ClientProperties = ClientProperties
    { product :: String
    } deriving (Show, Generic, Data, Typeable)

instance FromJSON ClientProperties

data Connection = Connection
    { name              :: String
    , user              :: String
    , recv_oct_details  :: TrafficDetails
    , send_oct_details  :: TrafficDetails
    , client_properties :: ClientProperties
    } deriving (Show, Generic, Data, Typeable)

instance FromJSON Connection

--
-- API
--

connections :: String -> IO (Maybe [Connection])
connections uri = do
    body <- getBody $ uri ++ "api/connections"
    return $ (decode' body :: Maybe [Connection])

--
-- Private
--
