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
  , cameraVMConfig
  , angle_x, angle_y
  )
  where

import           Ivory.Tower
import           Ivory.Language
import           Tower.AADL.Config

--------------------------------------------------------------------------------

cameraVMTower :: Tower e (ChanOutput (Struct "camera_angles"))
cameraVMTower = do
  towerModule  cameraVMModule
  towerDepends cameraVMModule

  (_            , fromVMRx)      <- channel
  (fromMonitorTx, fromMonitorRx) <- channel

  externalMonitor "camera_vm" $
    handler (fromVMRx :: ChanOutput (Struct "camera_angles")) "from_vm" $ do
      e <- emitter fromMonitorTx 1
      callback $ \msg -> emit e msg

  return fromMonitorRx

[ivory|
struct camera_angles {
  float angle_x;
  float angle_y;
}
|]

cameraVMModule :: Module
cameraVMModule = package "towerCameraVMDeps" $
  defStruct (Proxy :: Proxy "camera_angles")

cameraVMConfig :: AADLConfig
cameraVMConfig = defaultAADLConfig { configSystemHW       = ODROID
                                   , configCustomKConfig  = True
                                   , configCustomMakefile = True
                                   }
