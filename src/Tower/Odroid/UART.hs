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
  , uartConfig
  , UartPacket
  ) where

import           Ivory.Tower
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Artifact          as R
import qualified Ivory.Tower.HAL.Bus.Interface as I

import           Tower.AADL.Config

--------------------------------------------------------------------------------

uart :: String
uart = "uart"

[ivory| string struct UartPacket 255 |]

-- | Wrapper monitor. The string names (e.g., "uart") must be unique and
-- fixed.
uartTower :: IvoryString str
          => Tower e ( I.BackpressureTransmit str (Stored IBool)
                     , ChanOutput UartPacket)
uartTower
  = do
  towerModule  uartModule
  towerDepends uartModule

  -- From sender to translator
  req_chan  <- channel
  -- From translator to wrapper
  req_chan' <- channel
  -- Response
  resp_chan <- channel
  -- Received byte
  rx_chan   <- channel

  -- XXX We make a new monitor since if we just make this a handler in the
  -- external monitor, then the only channel with the type of an
  -- "ivory_string_UartPacket" is the outbound channel to the driver. But in
  -- Tower, channels going nowhere are dropped, so we can't collect its type.
  monitor "send_transdata" $ do
    -- Now just pass through values from driver.
    handler (snd req_chan) "send_translate" $ do
      e <- emitter (fst req_chan') 1
      callback $ \msg -> do
        msg' <- local (izero :: Init UartPacket)
        let srccap = arrayLen (msg ~> stringDataL)
        srclen <- msg ~>* stringLengthL
        assert $ srclen >=? 0 .&& srclen <=? srccap
        assert $ srccap <=? arrayLen (msg' ~> stringDataL)
        arrayCopy (msg' ~> stringDataL) (msg ~> stringDataL) 0 srclen
        store (msg' ~> stringLengthL) srclen
        emit e $ constRef msg'

  wrapperMonitor (snd req_chan') (fst resp_chan) (fst rx_chan)

  return (I.BackpressureTransmit (fst req_chan) (snd resp_chan), snd rx_chan)

-- The wrapper just passes the channel values through to and from the driver.
wrapperMonitor :: ChanOutput (Struct "ivory_string_UartPacket")
               -> ChanInput (Stored IBool)
               -> ChanInput UartPacket
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
      callback $ \msg -> emit e msg

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

uartConfig :: AADLConfig
uartConfig = defaultAADLConfig { configSystemHW = ODROID }
