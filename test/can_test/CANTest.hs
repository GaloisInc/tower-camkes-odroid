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

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Tower.AADL
import qualified Ivory.Tower.HAL.Bus.CAN as I
import qualified Ivory.Tower.HAL.Bus.Interface as I
import Tower.Odroid.CAN

--------------------------------------------------------------------------------

testCAN :: Tower e ()
testCAN = do
  towerModule  towerDepModule
  towerDepends towerDepModule

  (o, a) <- canTower
  per <- period (1000`ms`)

  monitor "sender" $ do
    abort_mode <- stateInit "abort_mode" (ival false)
    id_cnt <- stateInit "id" izero

    handler per "sendHandler" $ do
      reqChan   <- emitter (I.abortableTransmit a) 1
      abortChan <- emitter (I.abortableAbort a) 1
      callback $ \_per -> do
        am <- deref abort_mode
        ifte_ am
          (do call_ printf "Trying to abort\n"
              emitV abortChan true
          )
          ( do frame <- local (istruct [])
               ic <- deref id_cnt
               store (frame~>I.can_message_id) (I.standardCANID (fromRep ic) (fromRep 0))
               id_cnt += 1
               store (frame~>I.can_message_len) 8
               arrayMap $ \ix -> do
                 let v = castDefault (fromIx ix) + 1
                 store ((frame~>I.can_message_buf)!ix) v
               emit reqChan (constRef frame)
               i <- getStandardID frame
               call_ printf32 "Sent can frame with id %d, payload: " i
               arrayMap $ \ix -> do
                 p <- deref $ (frame~>I.can_message_buf)!ix
                 call_ printf8 "0x%02x " p
               call_ printf "\n"
               ic' <- deref id_cnt
               when (ic' >? 8) (store id_cnt 0)
          )
        abort_mode %= iNot

    handler (I.abortableComplete a) "statusHandler" $ do
      callback $ \msg -> do
        b <- deref msg
        call_ printfb "Sender received status: %u\n" b

  monitor "receiver"$ do
    handler o "rx_handler" $ do
      callback $ \frame -> do
        i <- getStandardID frame
        call_ printf32 "Recieved can frame with id %d, payload: " i
        arrayMap $ \ix -> do
          p <- deref $ (frame ~> I.can_message_buf)!ix
          call_ printf8 "0x%02x " p
        call_ printf "\n"

  where
  getStandardID frame = do
    arb <- frame ~>* I.can_message_id
    assert $ iNot $ bitToBool $ arb #. I.can_arbitration_ide
    assert $ iNot $ bitToBool $ arb #. I.can_arbitration_rtr
    return $ (toRep $ arb #. I.can_arbitration_id) `iShiftR` 18

--------------------------------------------------------------------------------
-- Compiler

main :: IO ()
main = compileTowerAADL id p testCAN
  where
  p _ = return canConfig

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

