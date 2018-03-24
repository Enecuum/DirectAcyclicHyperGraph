module RandHyper where

import HyperGraph

import System.Random
import Control.Monad (replicateM)

import qualified Data.IntMap as IM

import DirectHyperGraph (mkGraph)

genHyper n k = do
    let nodes = zip [1..n] (take n $ repeat ()) 
        g     = mkGraph nodes [] :: Sub () () ()
        h     = HGr g IM.empty
    gen h n k

gen hgr n k = do
    [n1,n2] <- replicateM 2 $ randomRIO (1,20)
    hp      <- replicateM n1 $ randomRIO (1,n)
    hs      <- replicateM n2 $ randomRIO (1,n) 
    let sub1  = mkGraph (zip hp (take n1 $ repeat ())) [] :: Sub () () () 
        sub2  = mkGraph (zip hs (take n2 $ repeat ())) [] :: Sub () () () 
        hyper = CDirect sub1 sub2 Block
        hgr'  = hyper +>> hgr
    if hsize hgr' >= k
    then return hgr' 
    else gen hgr' n k

-- test
htest = (genHyper 20 10) >>= hprettyPrint


