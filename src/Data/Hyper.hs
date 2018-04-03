module Data.Hyper 
( module H ) where

import Data.Hyper.HyperGraph as H
import Data.Hyper.PatriciaTree as H
import Data.Graph.Inductive.Graph as H

import Data.Version (showVersion)


-- | Version info
version :: IO ()
version =  undefined--putStrLn $ "\nFGL - Functional Graph Library, version "
                     