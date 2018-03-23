module RandHyper where

import HyperGraph

import System.Random
import Control.Monad (replicateM)

import qualified Data.IntMap as IM

import DirectHyperGraph (mkGraph)

nodes = zip [1..20] (take 20 $ repeat ()) 

g = mkGraph nodes [] :: Sub () () ()

h = HGr g IM.empty

gen hgr = do
    [n1,n2] <- replicateM 2 $ randomRIO (1,10)
    hp      <- replicateM n1 $ randomRIO (1,20)
    hs      <- replicateM n2 $ randomRIO (1,20) 
    let sub1  = mkGraph (zip hp (take n1 $ repeat ())) [] :: Sub () () () 
        sub2  = mkGraph (zip hs (take n2 $ repeat ())) [] :: Sub () () () 
        hyper = CDirect sub1 sub2 Block
        hgr'  = hyper +>> hgr
    if hsize hgr' >= 10 
    then return hgr' 
    else gen hgr'

-- test
hprint h = h >>= hprettyPrint


