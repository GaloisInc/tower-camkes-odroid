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
-- UART interface.
--
-- (c) 2015 Galois, Inc.
--
--------------------------------------------------------------------------------

module Tower.Odroid.UART
  ( uartTower
  , uartConfig
  , uartModule
  , UartPacket
  ) where

import           Ivory.Tower
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Artifact as R
import qualified Tower.AADL     as A
import qualified Ivory.Tower.HAL.Bus.Interface as I

import           System.FilePath
import qualified Paths_tower_camkes_odroid as P

--------------------------------------------------------------------------------

uart :: String
uart = "uart"

[ivory| string struct UartPacket 255 |]

-- | Wrapper monitor. The string names (e.g., "uart") must be unique and
-- fixed.
uartTower :: IvoryString str
          => Tower e ( I.BackpressureTransmit str (Stored IBool)
                     , ChanOutput (Stored Uint8))
uartTower
  = do
  towerModule  uartModule
  towerDepends uartModule

  -- From sender to wrapper
  req_chan  <- channel
  -- Response
  resp_chan <- channel
  -- Received byte
  rx_chan   <- channel

  wrapperMonitor (snd req_chan) (fst resp_chan) (fst rx_chan)

  return (I.BackpressureTransmit (fst req_chan) (snd resp_chan), snd rx_chan)

-- The wrapper just passes the channel values through to and from the driver.
wrapperMonitor :: IvoryString str
               => ChanOutput str
               -> ChanInput (Stored IBool)
               -> ChanInput (Stored Uint8)
               -> Tower e ()
wrapperMonitor req_chanRx resp_chanTx rx_chanTx = do

  -- From wrapper to driver. We're going to fix it's size to make the AADL
  -- wrapper easier to maintain.
  req_chan  <- channel
  -- Response from driver
  resp_chan <- channel
  -- Received byte
  rx_chan   <- channel

  externalMonitor uart $ do

    -- Now just pass through values from driver.
    handler req_chanRx "send" $ do
      e <- emitter (fst req_chan) 1
      callback $ \msg -> do
        msg' <- local (izero :: Init UartPacket)
        let srccap = arrayLen (msg ~> stringDataL)
        srclen <- msg ~>* stringLengthL
        assert $ srclen >=? 0 .&& srclen <=? srccap
        assert $ srccap <=? arrayLen (msg' ~> stringDataL)
        arrayCopy (msg' ~> stringDataL) (msg ~> stringDataL) 0 srclen
        store (msg' ~> stringLengthL) srclen
        emit e $ constRef msg'

    handler (snd rx_chan) "recv_rx" $ do
      e <- emitter rx_chanTx 1
      callback $ \msg -> emit e msg

    handler (snd resp_chan) "recv_resp" $ do
      e <- emitter resp_chanTx 1
      callback $ \msg -> emit e msg

--------------------------------------------------------------------------------

uartModule :: Module
uartModule = package "towerUartDeps" $
  defStruct (Proxy :: Proxy "ivory_string_UartPacket")

uartConfig :: A.Config
uartConfig = A.initialConfig
  { A.configSystemHW  = A.ODROID
  , A.configSystemOS  = A.CAmkES
  , A.configArtifacts = uartArtifacts
  }

uartArtifacts :: [R.Artifact]
uartArtifacts =
  [ a compDir (uart <.> "camkes")
  , a srcDir  "driver.c"
  ]
  where
  a d f   = R.artifactPath d
          $ R.artifactCabalFile P.getDataDir (uartDir </> d </> f)
  uartDir = "data/uart"
  compDir = "components" </> uart
  srcDir  = compDir </> "src"
