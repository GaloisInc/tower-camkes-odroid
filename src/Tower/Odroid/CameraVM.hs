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
  , cameraVMArtifacts
  , cameraVMConfig
  , left, right, top, bottom
  )
  where

import           Ivory.Tower
import           Ivory.Language
import           Ivory.Artifact          as R
import           Tower.AADL.Config

import           System.FilePath
import qualified Paths_tower_camkes_odroid as P

--------------------------------------------------------------------------------

cameraVMTower :: Tower e (ChanOutput (Struct "bbox"))
cameraVMTower = do
  towerModule  cameraVMModule
  towerDepends cameraVMModule
  mapM_ towerArtifact cameraVMArtifacts

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

cameraVMArtifacts :: [R.Located R.Artifact]
cameraVMArtifacts = (:[])
  $ R.Root
  $ R.artifactCabalFile P.getDataDir ("data" </> "camera_vm" </> "othercamkestargets.mk")

copyTargets :: (String, [String])
copyTargets = (,) rule
  [ rule ++ ":"
  , tab $ "mkdir -p components"
  , tab $ "cp -r $(SMACCM_PATH)/models/Trusted_Build_Test/camera_vm/components/camera_vm components/"
  , tab $ "cp -r $(SMACCM_PATH)/models/Trusted_Build_Test/camera_vm/components/VM components/"
  ]
  where
  rule = "copyVMcomponents"
  tab  = ('\t' :)

cameraVMConfig :: AADLConfig
cameraVMConfig = defaultAADLConfig { configSystemHW     = ODROID
                                   , configOtherTargets = [copyTargets]
                                   }
