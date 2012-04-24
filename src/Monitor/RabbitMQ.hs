module Monitor.RabbitMQ (
    overview
  ) where

import Monitor.RabbitMQ.Retrievable
import Monitor.RabbitMQ.Overview (Overview)

overview :: IO (Maybe Overview)
overview = retrieve (undefined :: Overview)