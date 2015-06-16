{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
------------------------------------------------------------------------------------
-- |
-- Module      :  HplAssets.Cloud.Types
-- Copyright   :  (c) authors and contributors, 2015
-- License     :  LGPL
--
--
-- This modules defines a cloud model. Basically, a cloud model comprises a 
-- list of clouds, where each cloud providers a set of instances 
-- (i.e., machine types). An instance has an id, a name, a location, 
-- a security group, a virtual machine image's id (image-id for short), and an 
-- access key. In this case, a security group represents the incoming and outgoing 
-- network traffic rules, whereas an access key represents the SSH key to connect 
-- to the virtual machines though SFTP. Additionally, a location comprises: 
-- (a) a geographic zone name (e.g., Europe, North America, among others), and (b) 
-- a zone (i.e., data center) where the machine is running on.
--
--
------------------------------------------------------------------------------------

module HplAssets.Cloud.Types
where

import GHC.Generics
import BasicTypes
import Data.Generics

data CloudTransformation = SelectAllClouds
           | SelectClouds [Id]
           | RemoveClouds [Id]
             deriving (Show, Eq, Ord)

data CloudModel = CloudModel 
  { clouds :: [Cloud]
  } deriving (Show, Eq, Data, Typeable)

data Cloud = Cloud 
  { cloudId   :: Id
  , name      :: String
  , instances :: [Instance]
  } deriving (Show, Data, Typeable)

data Instance = Instance 
  { instanceId    :: Id
  , instanceName  :: String
  , location      :: Location
  , securityGroup :: String
  , imageId       :: String
  , accessKey     :: String
  } deriving (Show, Data, Typeable)

data Location = Location 
  { region :: String
  , zone   :: String
  } deriving (Show, Data, Typeable, GHC.Generics.Generic)

--data HardwareDescription = HardwareDescription 
--  { computeUnit        :: ComputeUnit
--  , ramMemorySizeGB    :: Int
--  , networkDescription :: NetworkDescription
--  } deriving (Show, Eq, Data, Typeable, Generic)
--
--data ComputeUnit = ComputeUnit 
--  { gflops    :: Int
--  , frequency :: Int
--  , vcpus     :: Int
--  } deriving (Show, Eq, Data, Typeable, Generic)
--
--data NetworkDescription = NetworkDescription 
--  { throughputGbps :: Int
--  } deriving (Show, Eq, Data, Typeable, Generic)

-- The equality test of a cloud is based on its id.
instance Eq Cloud where
  x == y = cloudId x == cloudId y

-- The equality test of an instance is based on its id.
instance Eq Instance where
  x == y = instanceId x == instanceId y

-- The equality test of a location is based on its region and zone.
instance Eq Location where
    x == y = region x == region y && zone x == zone y
