{-# LANGUAGE OverloadedStrings #-}


module DumpKafkaRaw where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.Maybe
import Network.Kafka
import Network.Kafka.Consumer
import Network.Kafka.Protocol

import Blockchain.EthConf
import Blockchain.KafkaTopics

dumpKafkaRaw::Offset->IO ()
dumpKafkaRaw startingBlock = do
  ret <- runKafkaConfigured "queryStrato" $ doConsume' startingBlock
  case ret of
    Left e -> error $ show e
    Right _ -> return ()
  where
    doConsume' offset = do
              stateRequiredAcks .= -1
              stateWaitSize .= 1
              stateWaitTime .= 100000
              result <- fmap (map tamPayload . fetchMessages) $ fetch offset 0 (lookupTopic "block")

              liftIO $ putStrLn $ unlines $ map (BC.unpack . B16.encode) result

              doConsume' (offset + fromIntegral (length result))
