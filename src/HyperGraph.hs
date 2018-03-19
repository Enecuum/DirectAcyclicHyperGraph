{-# LANGUAGE GADTs, EmptyDataDecls#-}

module HyperGraph where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM 

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import DirectHyperGraph hiding (Edge)
import PatriciaTree

data Block h = Block
  deriving (Show)

data Simple

data Super

type Size = Int

-- HyperContext
data HContext h = SimpleHCont (IntSet, Block h) 
                | DirectHCont (IntSet, Block h, IntSet)
  deriving (Show)

type HGraphRep h = IntMap (HContext h)

-- two type of nodes of biparate graph 
data HNode a where
    HNode :: Node -> Node -> Simple -> HNode Simple
    --Block :: Node -> Node -> Super  -> HNode Super

type Sub a b h = Gr (Block h) a b 

-- HyperEdge representations
data Hyper a b h where
    Simple   :: [Node] -> h -> Hyper () () h
    SDirect  :: [Node] -> [Node] -> h -> Hyper () () h
    --Hyper :: (Gr sub) => (sub a b, sub a b) -> Edge a b
    Conected :: Sub a b h -> Block h -> Hyper a b h
    CDirect  :: Sub a b h -> Sub a b h -> Block h -> Hyper a b h
 
-- HyperGraph representations
data HGr a b h = --HGr (Gr h a b) (HGraphRep h)
                 HGr (Sub a b h) (HGraphRep h)
               | Shadow (Gr () a h)
  deriving (Show)

-- empty HyperGraph
hempty :: HGr a b h
hempty = HGr empty IM.empty

mkHyperGraph :: Hyper a b h -> HGr a b h
mkHyperGraph (Conected sub lab) = 
    let hedge  = IS.fromList (nodes sub)
        hcont  = mkHContInit hedge lab
        sub'   = insBlockInit sub lab
    in HGr sub hcont


-- add HyperEdge
(+>>) :: Hyper a b h -> HGr a b h -> HGr a b h
(Conected sub hl) +>> (HGr g hcont) = 
    let hedge  = IS.fromList (nodes sub)
        hcont' = mkHCont (IM.size hcont) hedge hl hcont
        g'     = mergeGraphs sub g
    in HGr g' hcont'


----------------------------------------------------------------------
-- UTILITIES
----------------------------------------------------------------------
mkHContInit :: IntSet -> Block h -> HGraphRep h
mkHContInit hedge h = IM.insert 0 (SimpleHCont (hedge, h)) IM.empty 

insBlockInit :: Sub a b h -> Block h -> Sub a b h
insBlockInit (Gr cont) hl = Gr (IM.map (f hl) cont)
  where f hl (ps, ss, l, hps, hss) = 
          let hps' = IM.insert 0 [hl] hps
          in (ps, ss, l, hps', hss) 

mergeGraphs sub g = insEdges (labEdges sub) $ insNodes (labNodes sub) g

mkHCont :: Int -> IntSet -> Block h -> HGraphRep h -> HGraphRep h
mkHCont n hedge h hcont = IM.insert n (SimpleHCont (hedge, h)) hcont


----------------------------------------------------------------------
-- EXAMPLES
----------------------------------------------------------------------

-- generate HyperEdges
ns0  = [(1,"a"), (2,"b")]
es0  = [(1,2,"ab")]
sub0 = mkGraph ns0 es0 :: Sub String String (Block ())
h0   = Conected sub0 Block
hgr0 = mkHyperGraph h0

ns1  = [(3,"c"), (4,"d")]
es1  = [(3,4,"cd")]
sub1 = mkGraph ns1 es1 :: Sub String String (Block ())
h1   = Conected sub1 Block
hgr1 = h1 +>> hgr0




















