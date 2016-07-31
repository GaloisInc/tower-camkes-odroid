{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , bbox_l, bbox_r, bbox_t, bbox_b
  )
  where

import           Ivory.Tower
import           Ivory.Language
import           Tower.AADL.Config
import           Tower.AADL.Platform

--------------------------------------------------------------------------------

cameraVMTower :: (IvoryArea a, IvoryZero a)
              => ChanOutput a
              -> Tower e (ChanOutput ('Struct "camera_data"))
cameraVMTower reboot_req = do
  towerModule  cameraVMModule
  towerDepends cameraVMModule

  (_            , fromVMRx     ) <- channel
  (toMonitorTx  , _            ) <- channel
  (fromMonitorTx, fromMonitorRx) <- channel

  externalMonitor "camera_vm" $ do
    -- camera data passthrough
    handler (fromVMRx :: ChanOutput ('Struct "camera_data")) "from_vm" $ do
      e <- emitter fromMonitorTx 1
      callback $ \msg -> emit e msg

    -- reboot request passthrough
    handler reboot_req "reboot_vm" $ do
      e <- emitter toMonitorTx 1
      callback $ \_ -> emitV e true

  return fromMonitorRx

[ivory|
struct camera_data {
  uint16_t bbox_l;
  uint16_t bbox_r;
  uint16_t bbox_t;
  uint16_t bbox_b;
}
|]

cameraVMModule :: Module
cameraVMModule = package "towerCameraVMDeps" $
  defStruct (Proxy :: Proxy "camera_data")

cameraVMConfig :: AADLConfig
cameraVMConfig = defaultAADLConfig { configSystemHW       = ODROID
                                   , configCustomKConfig  = True
                                   , configCustomMakefile = True
                                   }
