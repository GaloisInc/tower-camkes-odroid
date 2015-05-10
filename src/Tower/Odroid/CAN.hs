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

import           Control.Monad
import           System.FilePath
import qualified Paths_tower_camkes_odroid as P

--------------------------------------------------------------------------------

-- Corresponds to the can_message struct in tower-hal.
[ivory|
struct can_message
  { uint32_t  can_id -- can_message_id  :: Stored CANArbitrationField
  ; uint8_t   can_dlc -- can_message_len :: Stored (Ix 9)
  ;uint8_t[8] can_payload -- can_message_buf :: Array 8 (Stored Uint8)
  }
|]

mailboxes :: Int
mailboxes = 3

canTower ::
  Tower e ( ChanOutput (Struct "can_message")
          , AbortableTransmit (Struct "can_message") (Stored IBool)
          , AbortableTransmit (Struct "can_message") (Stored IBool)
          , AbortableTransmit (Struct "can_message") (Stored IBool)
          )
canTower
  = do
  towerModule  canModule
  towerDepends canModule

  recv <- channel
  -- reciver to driver
  recvW <- channel

  let idxs = [0 .. mailboxes]
  allChans <- replicateM mailboxes $ do
                send   <- channel
                abort  <- channel
                status <- channel
                return (send, abort, status)

  let hs = perMailboxHandlers (fst recv) recvW
  let toWrapperChans (send, abort, status) =
        (snd send, snd abort, fst status)

  let allWrapperChans = map toWrapperChans allChans
  handlers <- mapM hs (zip allWrapperChans idxs)

  externalMonitor "can_node" $ sequence_ (concat handlers)

  let as (send, abort, status) =
        AbortableTransmit (fst send) (fst abort) (snd status)
  let [a0, a1, a2] = map as allChans
  return (snd recv, a0, a1, a2)

perMailboxHandlers ::
     ChanInput (Struct "can_message")
  -> ( ChanInput  (Struct "can_message")
     , ChanOutput (Struct "can_message")
     )
  -> ( ( ChanOutput (Struct "can_message")
       , ChanOutput (Stored IBool)
       , ChanInput  (Stored IBool)
       )
     , Int
     )
  -> Tower e [Monitor e0 ()]
perMailboxHandlers recvTx recvW ((sendRx, abortRx, statusTx), i) = do

  -- To driver channels
  sendW   <- channel
  abortW  <- channel
  statusW <- channel

  return
    [
      -- Make the passthroughs.
      handler sendRx (idx "sender") $ do
        e <- emitter (fst sendW) 1
        callback $ \msg ->
          emit e msg

    , handler abortRx (idx "abortHandler") $ do
        e <- emitter (fst abortW) 1
        callback $ \msg ->
          emit e msg

    , handler (snd statusW) (idx "statusHandler") $ do
        e <- emitter statusTx 1
        callback $ \msg ->
          emit e msg

    , handler (snd recvW) (idx "recvHandler") $ do
        e <- emitter recvTx 1
        callback $ \msg ->
          emit e msg
    ]
  where
  idx s = s ++ show i

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
  map (a "include") (mkHdr include)
  ++ concatMap (uncurry putComponents)
      [ ("can", can)
      , ("can_node", can_node)
      , ("clk", clk)
      , ("gpio", gpio)
      , ("spi", spi)
      ]

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

