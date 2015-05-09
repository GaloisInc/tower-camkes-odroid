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
  ( -- canTower
  -- , 
    canConfig
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

[ivory|
struct can_message
  { uint32_t  id
  ; uint8_t   dlc
  ;uint8_t[8] can_payload
  }
|]

-- canTower :: Tower e ( ChanOutput (Struct "can_message")
--                     , I.AbortableTransmit (Struct "can_message") (Stored IBool)
--                     , I.AbortableTransmit (Struct "can_message") (Stored IBool)
--                     , I.AbortableTransmit (Struct "can_message") (Stored IBool)
--                     )
-- canTower
--   = do
--   towerModule  canModule
--   towerDepends canModule

--   -- From sender to wrapper
--   req_chan  <- channel
--   -- Response
--   resp_chan <- channel
--   -- Received byte
--   rx_chan   <- channel

--   return undefined

canConfig :: A.Config
canConfig = A.initialConfig
  { A.configSystemHW  = A.ODROID
  , A.configSystemOS  = A.CAmkES
  , A.configArtifacts = canArtifacts
  }

canModule :: Module
canModule = package "towerCanDeps" $ 
  defStruct (Proxy :: Proxy "can_message")

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
