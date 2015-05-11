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
  -- Can message fields
  , can_id
  , can_dlc
  , can_payload
  ) where

import           Ivory.Tower
import           Ivory.Language
import           Ivory.Artifact as R
import qualified Tower.AADL     as A
import           Ivory.Tower.HAL.Bus.Interface

import           System.FilePath
import qualified Paths_tower_camkes_odroid as P

--------------------------------------------------------------------------------

-- Corresponds to the can_message struct in tower-hal.
[ivory|
struct can_message
  { uint32_t  can_id      -- can_message_id  :: Stored CANArbitrationField
  ; uint8_t   can_dlc     -- can_message_len :: Stored (Ix 9)
  ; uint8_t[8] can_payload -- can_message_buf :: Array 8 (Stored Uint8)
  }
|]

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
canModule = package "towerCanDeps" $
  defStruct (Proxy :: Proxy "can_message")

--------------------------------------------------------------------------------
-- Artifacts

canArtifacts :: [R.Artifact]
canArtifacts =
--     a other other
     map (a "include") (mkHdr include)
  ++ concatMap (uncurry putComponents)
      [ ("can", can)
      , ("can_node", can_node)
      , ("clk", clk)
      , ("gpio", gpio)
      , ("spi", spi)
      ]
--  where other = "othercamkestargets.mk"

mkC :: [String] -> [FilePath]
mkC = map (<.> ".c")

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
  ]

can_node = (,)
  [ "can_node"
--  , "smaccm_can_node"
  ] []
  -- [ "smaccm_can_node"
  -- ]

clk = ([ "clk" ], [])

gpio = ([ "gpio" ], [])

spi = ([ "spi" ], [])

