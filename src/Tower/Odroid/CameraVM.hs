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
-- Camera VM interface.
--
-- (c) 2015 Galois, Inc.
--
--------------------------------------------------------------------------------

module Tower.Odroid.CameraVM
  ( cameraVMTower
  , cameraVMModule
  , left, right, top, bottom
  )
  where

import           Ivory.Tower
import           Ivory.Language

--------------------------------------------------------------------------------

cameraVMTower :: Tower e (ChanOutput (Struct "bbox"))
cameraVMTower = do
  towerModule  cameraVMModule
  towerDepends cameraVMModule

  (_            , fromVMRx)      <- channel
  (fromMonitorTx, fromMonitorRx) <- channel

  externalMonitor "camera_vm" $
    handler (fromVMRx :: ChanOutput (Struct "bbox")) "from_vm" $ do
      e <- emitter fromMonitorTx 1
      callback $ \msg -> emit e msg

  return fromMonitorRx

[ivory|
struct bbox {
  uint32_t left;
  uint32_t right;
  uint32_t top;
  uint32_t bottom;
}
|]

cameraVMModule :: Module
cameraVMModule = package "towerCameraVMDeps" $
  defStruct (Proxy :: Proxy "bbox")

