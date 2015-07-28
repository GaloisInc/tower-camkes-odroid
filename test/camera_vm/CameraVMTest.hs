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
       call_ printfFloat "left   %u\n" =<< deref (msg ~> angle_x)
       call_ printfFloat "right  %u\n" =<< deref (msg ~> angle_y)

--------------------------------------------------------------------------------
-- Compiler

main :: IO ()
main = compileTowerAADL id p testCameraVM
  where
  p _ = return cameraVMConfig

--------------------------------------------------------------------------------
-- Helpers

[ivory|
import (stdio.h, printf) void printfFloat(string x, float y)
|]

towerDepModule :: Module
towerDepModule = package "towerDeps" $ do
  incl printfFloat
