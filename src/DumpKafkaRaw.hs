{-# LANGUAGE OverloadedStrings #-}


module DumpKafkaRaw where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.Maybe
import Network.Kafka
import Network.Kafka.Consumer
import Network.Kafka.Producer
import Network.Kafka.Protocol
import System.Environment
import System.IO

fourth4::(a, b, c, d)->d
fourth4 (_, _, _, x) = x

fifth5::(a, b, c, d, e)->e
fifth5 (_, _, _, _, x) = x

dumpKafkaRaw = do
  _ <- runKafka (mkKafkaState "qqqqkafkaclientidqqqq" ("127.0.0.1", 9092)) doConsume'
  return ()
  where
    doConsume' = do
              stateRequiredAcks .= -1
              stateWaitSize .= 1
              stateWaitTime .= 100000
              --offset <- getLastOffset LatestTime 0 "thetopic"
              let offset = 0
              result <- fetch offset 0 "thetopic"


              let qq = concat $ map (map (_kafkaByteString . fromJust . _valueBytes . fifth5 . _messageFields .  _setMessage)) $ map _messageSetMembers $ map fourth4 $ head $ map snd $ _fetchResponseFields result
                                     
              liftIO $ putStrLn $ unlines $ map (BC.unpack . B16.encode) qq

              doConsume'
