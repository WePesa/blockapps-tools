{-# LANGUAGE OverloadedStrings #-}


module DumpKafkaBlocks where

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

fourth4::(a, b, c, d)->d
fourth4 (_, _, _, x) = x

fifth5::(a, b, c, d, e)->e
fifth5 (_, _, _, _, x) = x

dumpKafkaBlocks = do
  _ <- runKafka (mkKafkaState "queryStrato" ("127.0.0.1", 9092)) $ doConsume' 0
  return ()
  where
    doConsume' offset = do
      stateRequiredAcks .= -1
      stateWaitSize .= 1
      stateWaitTime .= 100000
      --offset <- getLastOffset LatestTime 0 "thetopic"
      result <- fetch offset 0 "thetopic"


      let retByteStrings = concat $ map (map (_kafkaByteString . fromJust . _valueBytes . fifth5 . _messageFields .  _setMessage)) $ map _messageSetMembers $ map fourth4 $ head $ map snd $ _fetchResponseFields result
                                     
      liftIO $ putStrLn $ unlines $ map format $ (map (rlpDecode . rlpDeserialize) retByteStrings::[Block])

      doConsume' (offset + fromIntegral (length retByteStrings))
