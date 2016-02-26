{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import System.Console.CmdArgs

import State
import Block
import BlockGO
import Hash
--import Init
import Code
import DumpKafkaBlocks
import Raw
import RLP
import RawMP


--import Debug.Trace



import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
--import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Blockchain.Database.MerklePatricia as MP


data Options = 
  State{root::String, db::String} 
  | Block{hash::String, db::String} 
  | BlockGO{hash::String, db::String} 
  | Hash{hash::String, db::String} 
  | Code{hash::String, db::String}
  | RawMP{stateRoot::String, filename::String}
  | Raw{filename::String}
  | RLP{filename::String}
  | Init{hash::String, db::String}
  | DumpKafkaBlocks{}
  deriving (Show, Data, Typeable)

stateOptions::Annotate Ann
stateOptions = 
  record State{root=undefined, db=undefined} [
    root := def += typ "SHAPtr" += argPos 1,
    db := def += typ "DBSTRING" += argPos 0
    ]

blockOptions::Annotate Ann
blockOptions = 
  record Block{hash=undefined, db=undefined} [
    hash := def += typ "FILENAME" += argPos 1 += opt ("-"::String),
    db := def += typ "DBSTRING" += argPos 0
    ]

blockGoOptions::Annotate Ann
blockGoOptions = 
  record BlockGO{hash=undefined, db=undefined} [
    hash := def += typ "FILENAME" += argPos 1 += opt ("-"::String),
    db := def += typ "DBSTRING" += argPos 0
    ]

hashOptions::Annotate Ann
hashOptions = 
  record Hash{hash=undefined, db=undefined} [
    hash := def += typ "FILENAME" += argPos 1 += opt ("-"::String),
    db := def += typ "DBSTRING" += argPos 0
    ]


initOptions::Annotate Ann
initOptions = 
  record Init{hash=undefined, db=undefined} [
    hash := def += typ "FILENAME" += argPos 1 += opt ("-"::String),
    db := def += typ "DBSTRING" += argPos 0
    ]

codeOptions::Annotate Ann
codeOptions = 
  record Code{hash=undefined, db=undefined} [
    hash := def += typ "USERAGENT" += argPos 1,
    db := def += typ "DBSTRING" += argPos 0
    ]

rawOptions::Annotate Ann
rawOptions = 
  record Raw{filename=undefined} [
    filename := def += typ "DBSTRING" += argPos 0
    ]

rlpOptions::Annotate Ann
rlpOptions = 
  record RLP{filename=undefined} [
    filename := def += typ "DBSTRING" += argPos 0
    ]

rawMPOptions::Annotate Ann
rawMPOptions = 
  record RawMP{stateRoot=undefined, filename=undefined} [
    stateRoot := def += typ "USERAGENT" += argPos 1,
    filename := def += typ "DBSTRING" += argPos 0
    ]

dumpKafkaBlocksOptions::Annotate Ann
dumpKafkaBlocksOptions =
  record DumpKafkaBlocks{} []

options::Annotate Ann
options = modes_ [stateOptions, blockOptions, blockGoOptions, hashOptions, initOptions, codeOptions, rawOptions, rlpOptions, rawMPOptions, dumpKafkaBlocksOptions]


--      += summary "Apply shims, reorganize, and generate to the input"


main::IO ()
main = do
  opts <- cmdArgs_ options
  run opts
    
-------------------


run::Options->IO ()

run State{root=r} = do
  let sr = MP.SHAPtr $ fst $ B16.decode $ BC.pack r
  State.doit sr

run Block{hash=h, db=db'} = do
  Block.doit db' h

run BlockGO{hash=h} = do
  BlockGO.doit h
         
run Hash{hash=h, db=db'} = do
  Hash.doit db' h

run Init{} = do
  undefined
--  Init.doit db' h

run Code{hash=h, db=db'} = do
  Code.doit db' h

run Raw{filename=f} = do
  Raw.doit f

run RLP{filename=f} = do
  RLP.doit f

run RawMP{stateRoot=sr, filename=f} = do
  RawMP.doit f (MP.SHAPtr $ fst $ B16.decode $ BC.pack sr)

run DumpKafkaBlocks{} = dumpKafkaBlocks
