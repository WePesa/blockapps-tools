{-# LANGUAGE OverloadedStrings #-}

module InsertTX where

import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Time.Clock
import qualified Database.Persist.Postgresql as SQL
import qualified Network.Haskoin.Crypto as H

import Blockchain.Data.Code
import Blockchain.Data.RawTransaction
import Blockchain.Data.Transaction
import Blockchain.Data.TXOrigin
import Blockchain.EthConf


Just prvKey = H.makePrvKey 0xabcd

insertTX::IO ()
insertTX = do
  theTime <- getCurrentTime
  db <- runNoLoggingT $ SQL.createPostgresqlPool connStr' 20
  tx <- H.withSource H.devURandom $ createContractCreationTX 0 1 1000000 0 (Code "abcd") prvKey
  --let tx = createMessageTX 0 1 1000000 (Address 0xabcd) 1 "" prvKey
  runResourceT $ flip SQL.runSqlPool db $ do
                        SQL.insert (txAndTime2RawTX Direct tx (-1) theTime)
                        return ()

