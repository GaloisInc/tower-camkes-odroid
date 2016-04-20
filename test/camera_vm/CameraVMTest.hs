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
-- Camera VM test.
--
-- (c) 2015 Galois, Inc.
--
--------------------------------------------------------------------------------

module Main where

import Ivory.Language
import Ivory.Tower
import Tower.AADL
import Tower.Odroid.CameraVM

--------------------------------------------------------------------------------

testCameraVM :: Tower e ()
testCameraVM = do
  towerModule  towerDepModule
  towerDepends towerDepModule

  reboot_chan <- channel
  rx <- cameraVMTower (snd reboot_chan :: ChanOutput ('Stored IBool))

  monitor "receiverMonitor" $
    handler rx "receiver" $
     callback $ \msg -> do
       call_ printf "left   %u\n" =<< deref (msg ~> bbox_l)
       call_ printf "right  %u\n" =<< deref (msg ~> bbox_r)
       call_ printf "top    %u\n" =<< deref (msg ~> bbox_t)
       call_ printf "bottom %u\n" =<< deref (msg ~> bbox_b)

--------------------------------------------------------------------------------
-- Compiler

main :: IO ()
main = compileTowerAADL id p testCameraVM
  where
  p _ = return cameraVMConfig

--------------------------------------------------------------------------------
-- Helpers

[ivory|
import (stdio.h, printf) void printf(string x, uint16_t y)
|]

towerDepModule :: Module
towerDepModule = package "towerDeps" $ do
  incl printf
