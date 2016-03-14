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
-- CAN interface.
--
-- (c) 2015 Galois, Inc.
--
--------------------------------------------------------------------------------

module Tower.Odroid.CAN
  ( canTower
  , canModule
  , canConfig
  ) where

import           Ivory.Tower
import           Ivory.Language
import           Ivory.Tower.HAL.Bus.CAN
import           Ivory.Tower.HAL.Bus.Interface

import           Tower.AADL.Config
import           Tower.AADL.Platform

--------------------------------------------------------------------------------

canTower ::
  Tower e ( ChanOutput ('Struct "can_message")
          , AbortableTransmit ('Struct "can_message") ('Stored IBool)
          )
canTower
  = do
  towerModule  canModule
  towerDepends canModule

  recv <- channel
  -- reciver to driver
  recvW <- channel

  send   <- channel
  abort  <- channel
  status <- channel

  let toWrapperChans = (snd send, snd abort, fst status)

  handlers <- perMailboxHandlers toWrapperChans

  externalMonitor "can_node" $ do
    sequence_ handlers
    handler (snd recvW) "recvHandler" $ do
      e <- emitter (fst recv) 1
      callback $ \msg ->
        emit e msg

  let as = AbortableTransmit (fst send) (fst abort) (snd status)
  return (snd recv, as)

perMailboxHandlers ::
  ( ChanOutput ('Struct "can_message")
  , ChanOutput ('Stored IBool)
  , ChanInput  ('Stored IBool)
  )
  -> Tower e [Monitor e0 ()]
perMailboxHandlers (sendRx, abortRx, statusTx) = do

  -- To driver channels
  sendW   <- channel
  abortW  <- channel
  statusW <- channel

  return
    [
      -- Make the passthroughs.
      handler sendRx "sender" $ do
        e <- emitter (fst sendW) 1
        callback $ \msg ->
          emit e msg

    , handler abortRx "abortHandler" $ do
        e <- emitter (fst abortW) 1
        callback $ \msg ->
          emit e msg

    , handler (snd statusW) "statusHandler" $ do
        e <- emitter statusTx 1
        callback $ \msg ->
          emit e msg
    ]


canModule :: Module
canModule = canDriverTypes

canConfig :: AADLConfig
canConfig = defaultAADLConfig { configSystemHW       = ODROID
                              , configCustomMakefile = True
                              }
