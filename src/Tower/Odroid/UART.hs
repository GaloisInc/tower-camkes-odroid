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
  , uartModule
  , UartPacket
  ) where

import           Ivory.Tower
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Artifact          as R
import           Ivory.Artifact.Location as R
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
  mapM_ towerArtifact uartArtifacts

  -------- To/From client --------
  -- Request
  req_chan0  <- channel
  -- Response
  resp_chan0 <- channel
  -- Received byte
  rx_chan0   <- channel

  -------- To/From external monitor --------
  -- Request
  req_chan1  <- channel
  -- Response
  resp_chan1 <- channel
  -- Received byte
  rx_chan1   <- channel

  monitor "uart_interface" $ do
    handler (snd req_chan0) "client_send" $ do
      e <- emitter (fst req_chan1) 1
      callback $ \msg -> do
        msg' <- local (izero :: Init UartPacket)
        let srccap = arrayLen (msg ~> stringDataL)
        srclen <- msg ~>* stringLengthL
        assert $ srclen >=? 0 .&& srclen <=? srccap
        assert $ srccap <=? arrayLen (msg' ~> stringDataL)
        arrayCopy (msg' ~> stringDataL) (msg ~> stringDataL) 0 srclen
        store (msg' ~> stringLengthL) srclen
        emit e $ constRef msg'

    handler (snd resp_chan1) "client_resp" $ do
      e <- emitter (fst resp_chan0) 1
      callback $ \msg -> emit e msg

    handler (snd rx_chan1) "client_rx" $ do
      e <- emitter (fst rx_chan0) 1
      callback $ \msg -> emit e msg

  wrapperMonitor (snd req_chan1) (fst resp_chan1) (fst rx_chan1)

  return (I.BackpressureTransmit (fst req_chan0) (snd resp_chan0), snd rx_chan0)

-- The wrapper just passes the channel values through to and from the driver.
wrapperMonitor :: ChanOutput (Struct "ivory_string_UartPacket")
               -> ChanInput (Stored IBool)
               -> ChanInput (Stored Uint8)
               -> Tower e ()
wrapperMonitor req_chanRx resp_chanTx rx_chanTx = do

  -------- To/From external driver --------
  -- Request
  req_chan1  <- channel
  -- Response
  resp_chan1 <- channel
  -- Received byte
  rx_chan1   <- channel

  externalMonitor uart $ do

    -- Now just pass through values from driver.
    handler req_chanRx "send" $ do
      e <- emitter (fst req_chan1) 1
      callback $ \msg -> emit e msg

    handler (snd rx_chan1) "recv_rx" $ do
      e <- emitter rx_chanTx 1
      callback $ \msg -> emit e msg

    handler (snd resp_chan1) "recv_resp" $ do
      e <- emitter resp_chanTx 1
      callback $ \msg -> emit e msg

--------------------------------------------------------------------------------

uartModule :: Module
uartModule = package "towerUartDeps" $
  defStruct (Proxy :: Proxy "ivory_string_UartPacket")

uartArtifacts :: [R.Located R.Artifact]
uartArtifacts = map R.Root
  [ a compDir (uart <.> "camkes")
  , a srcDir  "driver.c"
  ]
  where
  a d f   = R.artifactPath d
          $ R.artifactCabalFile P.getDataDir (uartDir </> d </> f)
  uartDir = "data/uart"
  compDir = "components" </> uart
  srcDir  = compDir </> "src"
