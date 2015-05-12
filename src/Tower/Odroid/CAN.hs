{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- CAN interface.
--
-- (c) 2015 Galois, Inc.
--
--------------------------------------------------------------------------------

module Tower.Odroid.CAN
  ( canTower
  , canArtifacts
  , canConfig
  , canModule
  ) where

import           Ivory.Tower
import           Ivory.Language
import           Ivory.Artifact as R
import qualified Tower.AADL     as A
import           Ivory.Tower.HAL.Bus.CAN
import           Ivory.Tower.HAL.Bus.Interface

import           System.FilePath
import qualified Paths_tower_camkes_odroid as P

--------------------------------------------------------------------------------

canTower ::
  Tower e ( ChanOutput (Struct "can_message")
          , AbortableTransmit (Struct "can_message") (Stored IBool)
          )
canTower
  = do
  towerModule  canModule
  towerDepends canModule

  recv <- channel
  -- reciver to driver
  recvW <- channel

  send   <- channel
  abort  <- channel
  status <- channel

  let toWrapperChans = (snd send, snd abort, fst status)

  handlers <- perMailboxHandlers toWrapperChans

  externalMonitor "can_node" $ do
    sequence_ handlers
    handler (snd recvW) "recvHandler" $ do
      e <- emitter (fst recv) 1
      callback $ \msg ->
        emit e msg

  let as = AbortableTransmit (fst send) (fst abort) (snd status)
  return (snd recv, as)

perMailboxHandlers ::
  ( ChanOutput (Struct "can_message")
  , ChanOutput (Stored IBool)
  , ChanInput  (Stored IBool)
  )
  -> Tower e [Monitor e0 ()]
perMailboxHandlers (sendRx, abortRx, statusTx) = do

  -- To driver channels
  sendW   <- channel
  abortW  <- channel
  statusW <- channel

  return
    [
      -- Make the passthroughs.
      handler sendRx "sender" $ do
        e <- emitter (fst sendW) 1
        callback $ \msg ->
          emit e msg

    , handler abortRx "abortHandler" $ do
        e <- emitter (fst abortW) 1
        callback $ \msg ->
          emit e msg

    , handler (snd statusW) "statusHandler" $ do
        e <- emitter statusTx 1
        callback $ \msg ->
          emit e msg
    ]

canConfig :: A.Config
canConfig = A.initialConfig
  { A.configSystemHW  = A.ODROID
  , A.configSystemOS  = A.CAmkES
  , A.configArtifacts = canArtifacts
  }

canModule :: Module
canModule = canDriverTypes

--------------------------------------------------------------------------------
-- Artifacts

canArtifacts :: [R.Artifact]
canArtifacts =
     a "" other
   : map (a "include") (mkHdr include)
  ++ map (a "interfaces") (mkIdl interfaces)
  ++ concatMap (uncurry putComponents)
      [ ("can", can)
      , ("can_node", can_node)
      , ("clk", clk)
      , ("gpio", gpio)
      , ("spi", spi)
      ]
  where other = "othercamkestargets.mk"

mkC :: [String] -> [FilePath]
mkC = map (<.> ".c")

mkIdl :: [String] -> [FilePath]
mkIdl = map (<.> ".idl4")

mkHdr :: [String] -> [FilePath]
mkHdr = map (<.> ".h")

mkCamkes :: String -> FilePath
mkCamkes nm = nm <.> "camkes"

putComponents :: String
              -> ([FilePath],[FilePath])
              -> [R.Artifact]
putComponents nm (srcs, hdrs) =
    a compDir (mkCamkes nm)
  : map (a (compDir </> "src")) (mkC srcs)
 ++ map (a (compDir </> "include")) (mkHdr hdrs)
  where
  compDir = "components" </> nm

dataDir :: FilePath
dataDir = "data/can"

a :: FilePath -> FilePath -> Artifact
a d f = R.artifactPath d
      $ R.artifactCabalFile P.getDataDir (dataDir </> d </> f)

interfaces :: [String]
interfaces = ["can_tx", "can_rx", "gpio", "clk", "spi"]

include :: [String]
include =
  [ "can_inf"
  , "common"
  , "spi_inf"
  , "utils"
  ]

can, can_node, clk, gpio, spi :: ([String], [String])
can = (,)
  [ "controller"
  , "dev"
  , "irq"
  , "prio"
  , "queue"
  , "spi_cmd"
  ]
  [ "mcp2515"
  , "queue"
  , "can_inf"
  ]

can_node = (,)
  [ "can_node"
  ]
  [ "can_inf"
  ]

clk = ([ "clk" ], [])

gpio = ([ "gpio" ], [])

spi = ([ "spi" ], [])

