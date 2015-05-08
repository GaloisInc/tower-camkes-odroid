{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- UART interface.
--
-- (c) 2015 Galois, Inc.
--
--------------------------------------------------------------------------------

module Tower.Odroid.Serial
  ( uartTower
  , uart_packet_payload
  , uart_packet_len
  , uartConfig
  , uartModule
  ) where

import           Ivory.Tower
import           Ivory.Language
import           Ivory.Artifact as R
import qualified Tower.AADL     as A

import           System.FilePath
import qualified Paths_tower_camkes_odroid as P

--------------------------------------------------------------------------------

[ivory|

struct uart_packet
  { uint8_t[255]  uart_packet_payload
  ; uint8_t       uart_packet_len
  }
|]

uart :: String
uart = "uart"

-- | Wrapper monitor. The string names (e.g., "uart") must be unique and
-- fixed.
uartTower
  :: ChanOutput (Struct "uart_packet")
  -- ^ Output from client
  -> ChanInput  (Stored Uint8)
  -- ^ Input to client
  -> ChanInput  (Stored IBool)
  -- ^ Input to client
  -> Tower e ()
uartTower s2wRx w2rTx notifyTx
  = do
  towerModule  uartModule
  towerDepends uartModule

  -- From wrapper to driver
  (w2dTx, _ ) <- channel
  -- From driver to wrapper
  (_, d2wRx :: ChanOutput (Stored Uint8)) <- channel

  externalMonitor uart $ do
    -- Rx from sender, define a symbol for handling msg, but don't implement it.
    handler s2wRx "send" $ do
      e <- emitter w2dTx 1
      b <- emitter notifyTx 1
      callback $ \msg -> do
        emit e msg
        emitV b true

    -- From driver
    handler d2wRx "recv" $ do
      e <- emitter w2rTx 1
      callback $ \msg -> emit e msg

--------------------------------------------------------------------------------

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
  , a srcDir  ("smaccm_uart" <.> "c")
  ]
  where
  a d f   = R.artifactPath d
          $ R.artifactCabalFile P.getDataDir (uartDir </> d </> f)
  uartDir = "data/uart"
  compDir = "components" </> uart
  srcDir  = compDir </> "src"

uartModule :: Module
uartModule = package "towerUartDeps" $
  defStruct (Proxy :: Proxy "uart_packet")
