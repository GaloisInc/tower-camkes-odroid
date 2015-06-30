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

  rx <- cameraVMTower

  monitor "receiverMonitor" $
    handler rx "receiver" $
     callback $ \msg -> do
       call_ printf32 "left   %u\n" =<< deref (msg ~> left)
       call_ printf32 "right  %u\n" =<< deref (msg ~> right)
       call_ printf32 "top    %u\n" =<< deref (msg ~> top)
       call_ printf32 "bottom %u\n" =<< deref (msg ~> bottom)

--------------------------------------------------------------------------------
-- Compiler

main :: IO ()
main = compileTowerAADL id p testCameraVM
  where
  p _ = return cameraVMConfig

--------------------------------------------------------------------------------
-- Helpers

[ivory|
import (stdio.h, printf) void printf32(string x, uint32_t y)
|]

towerDepModule :: Module
towerDepModule = package "towerDeps" $ do
  incl printf32
