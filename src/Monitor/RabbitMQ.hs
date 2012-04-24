module Monitor.RabbitMQ (
    overview
  ) where

import Monitor.Uri               (getEnvUri)
import Monitor.RabbitMQ.Retrievable
import Monitor.RabbitMQ.Overview (Overview)

overview :: IO (Maybe Overview)
overview = do
    uri <- getEnvUri "RABBITMQ_MGMT_URI"
    retrieve uri (undefined :: Overview)