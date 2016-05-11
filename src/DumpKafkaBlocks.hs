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
import Blockchain.Stream.VMEvent
import Blockchain.KafkaTopics
import Blockchain.EthConf

dumpKafkaBlocks::Offset->IO ()
dumpKafkaBlocks startingBlock = do
  ret <- runKafkaConfigured "queryStrato" $ doConsume' startingBlock
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
