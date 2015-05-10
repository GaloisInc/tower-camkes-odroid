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
-- CAN client example, corresponding to
-- smaccm/models/Trusted_Build_Test/can.

--
-- (c) 2015 Galois, Inc.
--
--------------------------------------------------------------------------------

module Main where

import System.Environment

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Tower.AADL
import qualified Ivory.Tower.HAL.Bus.Interface as I
import Tower.Odroid.CAN

--------------------------------------------------------------------------------

testCAN :: Tower e ()
testCAN = do
  towerModule  towerDepModule
  towerDepends towerDepModule

  (o, a0, _a1, _a2) <- canTower
  per <- period (1000`ms`)

  monitor "sender" $ do
    abort_mode <- stateInit "abort_mode" (ival false)
    id_cnt <- stateInit "id" izero

    handler per "sendHandler" $ do
      reqChan   <- emitter (I.abortableTransmit a0) 1
      abortChan <- emitter (I.abortableAbort a0) 1
      callback $ \_per -> do
        am <- deref abort_mode
        ifte_ am
          (do call_ printf "Trying to abort\n"
              emitV abortChan true
          )
          ( do frame <- local (istruct [])
               ic <- deref id_cnt
               store (frame~>can_id) ic
               id_cnt += 1
               arrayMap $ \ix -> do
                 let v = castDefault (fromIx ix) + 1
                 store ((frame~>can_payload)!ix) v
               emit reqChan (constRef frame)
               i <- frame~>*can_id
               call_ printf32 "Sent can frame with id %d, payload: " i
               arrayMap $ \ix -> do
                 p <- deref $ (frame~>can_payload)!ix
                 call_ printf8 "0x%02x " p
               call_ printf "\n"
               ic' <- deref id_cnt
               when (ic' >? 8) (store id_cnt 0)
          )
        abort_mode %= iNot

    handler (I.abortableComplete a0) "statusHandler" $ do
      callback $ \msg -> do
        b <- deref msg
        call_ printfb "Sender received status: %u\n" b

  monitor "receiver"$ do
    handler o "rx_handler" $ do
      callback $ \frame -> do
        i <- deref (frame ~> can_id)
        call_ printf32 "Recieved can frame with id %d, payload: " i
        arrayMap $ \ix -> do
          p <- deref $ (frame ~> can_payload)!ix
          call_ printf8 "0x%02x " p
        call_ printf "\n"

--------------------------------------------------------------------------------
-- Compiler

main :: IO ()
main = do
  args <- getArgs
  opts <- parseOpts args
  runCompileAADL opts canConfig testCAN

--------------------------------------------------------------------------------
-- Helpers

[ivory|
import (stdio.h, printf) void printf(string x)
import (stdio.h, printf) void printfb(string x, bool y)
import (stdio.h, printf) void printf8(string x, uint8_t y)
import (stdio.h, printf) void printf32(string x, uint32_t y)
|]

towerDepModule :: Module
towerDepModule = package "towerDeps" $ do
  incl printf
  incl printfb
  incl printf8
  incl printf32
  depend canModule

