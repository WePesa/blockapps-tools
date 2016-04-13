{-# LANGUAGE OverloadedStrings #-}


module DumpKafkaBlocks where

import Control.Lens
import Control.Monad.IO.Class
import Network.Kafka
import Network.Kafka.Protocol
import qualified Data.Map as M
import Data.Maybe
import qualified Data.ByteString.Char8 as BC

import Blockchain.Data.BlockDB
import Blockchain.Format
import Blockchain.KafkaTopics

dumpKafkaBlocks::Offset->IO ()
dumpKafkaBlocks startingBlock = do
  let maybeBlockappsDataTopic = M.lookup "queryStrato" kafkaTopics
      kafkaString = KString . BC.pack $ fromMaybe "queryStrato" maybeBlockappsDataTopic

  ret <- runKafka (mkKafkaState kafkaString ("127.0.0.1", 9092)) $ doConsume' startingBlock
  case ret of
    Left e -> error $ show e
    Right _ -> return ()
  where
    doConsume' offset = do
      stateRequiredAcks .= -1
      stateWaitSize .= 1
      stateWaitTime .= 100000
      blocks <- fetchBlocks offset
                                     
      liftIO $ putStrLn $ unlines $ map format blocks

      doConsume' (offset + fromIntegral (length blocks))
