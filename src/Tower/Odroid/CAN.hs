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
  , canConfig
  , canModule
  ) where

import           Ivory.Tower
import           Ivory.Language
import           Ivory.Artifact as R
import qualified Tower.AADL     as A
import qualified Ivory.Tower.HAL.Bus.Interface as I

import           System.FilePath
import qualified Paths_tower_camkes_odroid as P

--------------------------------------------------------------------------------

canTower :: Tower e ( I.BackpressureTransmit s (Stored IBool)
                     , ChanOutput (Stored Uint8))
canTower
 = do undefined

canConfig :: A.Config
canConfig = A.initialConfig
  { A.configSystemHW  = A.ODROID
  , A.configSystemOS  = A.CAmkES
  , A.configArtifacts = canArtifacts
  }

canModule :: Module
canModule = package "towerCanDeps" $ undefined
--  defStruct (Proxy :: Proxy "ivory_string_UartPacket")

canArtifacts :: [R.Artifact]
canArtifacts = []
  -- [ a compDir (uart <.> "camkes")
  -- , a srcDir  "driver.c"
  -- ]
  -- where
  -- a d f   = R.artifactPath d
  --         $ R.artifactCabalFile P.getDataDir (uartDir </> d </> f)
  -- uartDir = "data/uart"
  -- compDir = "components" </> uart
  -- srcDir  = compDir </> "src"
