{-# LANGUAGE OverloadedStrings #-}


module DumpKafkaUnminedBlocks where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Network.Kafka
import Network.Kafka.Consumer
import Network.Kafka.Producer
import Network.Kafka.Protocol
import System.Environment
import System.IO

import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
import Blockchain.Format

dumpKafkaUnminedBlocks startingBlock = do
  ret <- runKafka (mkKafkaState "queryStrato" ("127.0.0.1", 9092)) $ doConsume' startingBlock
  case ret of
    Left e -> error $ show e
    Right v -> return ()
  where
    doConsume' offset = do
      stateRequiredAcks .= -1
      stateWaitSize .= 1
      stateWaitTime .= 100000
      blocks <- fetchUnminedBlocks offset
                                     
      liftIO $ putStrLn $ unlines $ map format blocks

      doConsume' (offset + fromIntegral (length blocks))
