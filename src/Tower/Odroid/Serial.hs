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
  , uart_num
  , datum
  , uartConfig
  ) where

import           Ivory.Tower
import           Ivory.Language
import           Ivory.Artifact as R
import qualified Tower.AADL     as A

import           System.FilePath
import qualified Paths_tower_camkes_odroid as P

--------------------------------------------------------------------------------

-- Interface struct
[ivory|
struct uart_packet
  { int32_t uart_num
  ; uint8_t   datum
  }
|]

uart_monitor :: String
uart_monitor = "uart_monitor"

-- | Wrapper monitor. The string names (e.g., "uart_monitor") must be unique and
-- fixed.
uartTower
  :: ChanOutput (Struct "uart_packet")
  -- ^ Output from client
  -> ChanInput  (Struct "uart_packet")
  -- ^ Input to client
  -> Tower e ()
uartTower s2wRx w2rTx
  = do
  towerModule  uartModule
  towerDepends uartModule

  -- From wrapper to driver
  (w2dTx, _ ) <- channel
  -- From driver to wrapper
  (_, d2wRx :: ChanOutput (Struct "uart_packet")) <- channel

  externalMonitor uart_monitor $ do
    -- Rx from sender, define a symbol for handling msg, but don't implement it.
    handler s2wRx "send" $ do
      e <- emitter w2dTx 1
      callback $ \msg -> emit e msg

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
  [ a compDir (uart_monitor <.> "camkes")
  , a srcDir  "driver.c"
  , a srcDir  ("smaccm_uart_monitor" <.> "c")
  ]
  where
  a d f   = R.artifactPath d
          $ R.artifactCabalFile P.getDataDir (uartDir </> d </> f)
  uartDir = "data/uart"
  compDir = "components" </> uart_monitor
  srcDir  = compDir </> "src"

uartModule :: Module
uartModule = package "towerUartDeps" $
  defStruct (Proxy :: Proxy "uart_packet")
