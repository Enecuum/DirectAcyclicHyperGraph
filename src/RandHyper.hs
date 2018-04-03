module RandHyper where

import Data.Hyper.HyperGraph

import System.Random
import Control.Monad (replicateM)

import qualified Data.IntMap as IM (empty)
import qualified Data.IntSet as IS 

import Data.Graph.Inductive.Graph (mkGraph)

genHyper n k = do
    let nodes = zip [1..n] (take n $ repeat ()) 
        g     = mkGraph nodes [] :: Sub () () ()
        h     = HGr g IM.empty
    gen h n k

gen hgr n k = do
    [n1,n2] <- replicateM 2 $ randomRIO (1,n)
    hp      <- replicateM n1 $ randomRIO (1,n)
    hs1     <- replicateM n2 $ randomRIO (1,n) 
    let hp'   = IS.fromList hp
        hs'   = IS.fromList hs1
        hs2   = IS.toList $ IS.difference hs' (IS.intersection hp' hs')  
        sub1  = mkGraph (zip hp (take n1 $ repeat ())) [] :: Sub () () () 
        sub2  = mkGraph (zip hs2 (take n2 $ repeat ())) [] :: Sub () () () 
        hyper = CDirect sub1 sub2 Block
        hgr'  = hyper +>> hgr
    if hsize hgr' >= k
    then return hgr' 
    else gen hgr' n k

-- test
htest = (genHyper 20 10) >>= hprettyPrint


