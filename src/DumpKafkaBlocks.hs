{-# LANGUAGE OverloadedStrings #-}


module DumpKafkaBlocks where

import Control.Lens
import Control.Monad.IO.Class
import Network.Kafka
import Network.Kafka.Protocol

import Blockchain.Format
import Blockchain.Stream.VMEvent

dumpKafkaBlocks::Offset->IO ()
dumpKafkaBlocks startingBlock = do
  ret <- runKafka (mkKafkaState "queryStrato" ("127.0.0.1", 9092)) $ doConsume' startingBlock
  case ret of
    Left e -> error $ show e
    Right _ -> return ()
  where
    doConsume' offset = do
      stateRequiredAcks .= -1
      stateWaitSize .= 1
      stateWaitTime .= 100000
      vmEvents <- fetchVMEvents offset
                                     
      liftIO $ putStrLn $ unlines $ map format vmEvents

      doConsume' (offset + fromIntegral (length vmEvents))
