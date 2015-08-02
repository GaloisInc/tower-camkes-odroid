{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- UART client example, corresponding to
-- smaccm/models/Trusted_Build_Test/test_uart_active2.

--
-- (c) 2015 Galois, Inc.
--
--------------------------------------------------------------------------------

module Main where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Tower.AADL
import qualified Ivory.Tower.HAL.Bus.Interface as I
import Tower.Odroid.UART

--------------------------------------------------------------------------------

testSerial :: Tower e ()
testSerial = do
  towerModule  towerDepModule
  towerDepends towerDepModule

  per <- period (2000`ms`)

  -- Driver wrapper
  (b, o) <- uartTower

  monitor "sender" $ do
    c <- stateInit "charState" (ival 65) -- 'A'
    packet <- stateInit "packet" (izero :: Init UartPacket)

    handler per "periodicHandler" $ do
      e <- emitter (I.backpressureTransmit b) 1 -- Send to wrapper
      callback $ \_msg -> do
        for 5 $ \ix -> do
          let arr = packet ~> stringDataL
          c' <- deref c
          store (arr!ix) c'
          call_ printf2 "Sending code: 0x%x --> %c\n" c' c'
          ifte_ (c' >? 90) -- 'Z'
                (store c 65)
                (c += 1)
        store (packet ~> stringLengthL) 5
        call_ printf0 "Sent!\n"
        emit e (constRef packet)

    handler (I.backpressureComplete b) "resp" $ do
      callback $ \_msg -> do
        call_ printf0 "Received response.\n"

  monitor "receiver" $ do
    handler o "receiverHandler" $ do
      callback $ \msg -> do -- Receive from wrapper
        len <-  msg ~>* stringLengthL
        let d = msg ~>  stringDataL
        arrayMap $ \ix -> do
          when (fromIx ix <? len) $
            call_ printf1 "Received input: %c\n" =<< deref (d!ix)

--------------------------------------------------------------------------------
-- Compiler

main :: IO ()
main = compileTowerAADL id p testSerial
  where
  p _ = return uartConfig

--------------------------------------------------------------------------------
-- Helpers

[ivory|
import (stdio.h, printf) void printf0(string x)
import (stdio.h, printf) void printf1(string x, uint8_t y)
import (stdio.h, printf) void printf2(string x, uint8_t y, uint8_t z)
|]

towerDepModule :: Module
towerDepModule = package "towerDeps" $ do
  incl printf0
  incl printf1
  incl printf2
  depend uartModule
