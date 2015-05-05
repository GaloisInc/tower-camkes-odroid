{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- UART Client.
--
-- (c) 2015 Galois, Inc.
--
--------------------------------------------------------------------------------

module Tower.Odroid.Serial
  ( uartMonitor
  , uart_num
  , datum
  ) where

import Ivory.Tower
import Ivory.Language

--------------------------------------------------------------------------------
-- Types

[ivory|
struct uart_packet
  { uint8_t uart_num
  ; int64_t datum
  }
|]

--------------------------------------------------------------------------------

-- | Wrapper monitor
uartMonitor
  :: ChanOutput (Struct "uart_packet")
  -> ChanInput  (Struct "uart_packet")
  -> Tower e ()
uartMonitor
  s2wRx
  -- ^ sender to wrapper (rx end)
  w2rTx
  -- ^ wrapper to receiver (tx end)
  = do
  -- From wrapper to driver
  (w2dTx, _ ) <- channel
  -- From driver to wrapper
  (_, d2wRx :: ChanOutput (Struct "uart_packet")) <- channel

  externalMonitor "uart_monitor" $ do
    -- Rx from sender, define a symbol for handling msg, but don't implement it.
    handler s2wRx "send" $ do
      e <- emitter w2dTx 1
      callback $ \msg -> emit e msg

    -- From driver
    handler d2wRx "recv" $ do
      e <- emitter w2rTx 1
      callback $ \msg -> emit e msg
