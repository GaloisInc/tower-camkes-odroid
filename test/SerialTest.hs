{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PostfixOperators #-}

--------------------------------------------------------------------------------
-- UART client example, corresponding to
-- smaccm/models/Trusted_Build_Test/test_uart_active2.

--
-- (c) 2015 Galois, Inc.
--
--------------------------------------------------------------------------------

module Main where

import System.Environment
import Data.Maybe

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Tower.AADL
import Tower.Odroid.Serial

testSerial :: Tower e ()
testSerial = do
  towerModule  towerDepModule
  towerDepends towerDepModule

  per <- period (2000`ms`)

  -- From user code sender to wrapper
  (s2wTx, s2wRx) <- channel
  -- From wrapper to user code receiver
  (w2rTx, w2rRx) <- channel

  -- Driver wrapper
  uartTower s2wRx w2rTx

  monitor "sender" $ do
    c <- stateInit "charState" (ival 65) -- 'A'
    packet <- stateInit "packet" izero
    handler per "periodicHandler" $ do
      e <- emitter s2wTx 1 -- Send to wrapper
      callback $ \_msg -> do
        c' <- deref c
        call_ send packet c'
        emit e (constRef packet)
        ifte_ (c' >? 90) -- 'Z'
              (store c 65)
              (c += 1)

  monitor "receiver" $ do
    handler w2rRx "receiverHandler" $ do
      callback $ \msg -> do -- Receive from wrapper
        call_ receive msg

-- user_sender.c
send :: Def('[Ref s (Struct "uart_packet"), Uint8] :-> ())
send = proc "send" $ \packet c -> body $ do
  store (packet ~> uart_num) 0
  store (packet ~> datum) c
  d <- deref (packet ~> datum)
  call_ printf2 "Sending code: 0x%x --> %c\n" d d

-- user_receiver.c
receive :: Def('[ConstRef s (Struct "uart_packet")] :-> ())
receive = proc "receive" $ \input1 -> body $ do
  d <- input1 ~>* datum
  call_ printf1 "Received input: 0x%x --> %c\n" d

--------------------------------------------------------------------------------
-- Compiler

main :: IO ()
main = do
  args <- getArgs
  opts <- parseOpts args
  runCompileAADL
    opts  { genDirOpts = if isNothing (genDirOpts opts)
                           then Just "out/testUart"
                           else genDirOpts opts
          , configOpts =
              configOpts opts `appendArtifacts` uartConfig
          }
    testSerial

--------------------------------------------------------------------------------
-- Helpers

[ivory|
import (stdio.h, printf) void printf1(string x, uint8_t y)
import (stdio.h, printf) void printf2(string x, uint8_t y, uint8_t z)
|]

towerDepModule :: Module
towerDepModule = package "towerDeps" $ do
  incl printf1
  incl printf2
  incl send
  incl receive
