{-# LANGUAGE GADTs, EmptyDataDecls#-}

module HyperGraph where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM 

import DirectHyperGraph hiding (Edge)
import PatriciaTree

--type Context' a b = (IntMap [b], a, IntMap [b])

--type GraphRep a b = IntMap (Context' a b)

data Block

data Simple

data Super

type Size = Int

-- HyperContext
data HContext a b h where 
    SimpleCont   :: (h, [Node]) -> HContext () () h
    SDirectCont  :: ([Node], h, [Node]) -> HContext () () h
    ConectedCont :: (Gr () a b, h) -> HContext a b h
    CDirectCont  :: (Gr () a b, h, Gr () a b) -> HContext a b h 

type HGraphRep a b h = IntMap (HContext a b h)

--type HContext' a b h = (IntMap [b], IntMap [b], a, IntMap [h], IntMap [h])

--type HGraphRep a b h = IntMap (HContext' a b h)

-- two type of nodes of biparate graph 
data HNode a where
    HNode :: Node -> Node -> Simple -> HNode Simple
    Block :: Node -> Node -> Super  -> HNode Super

-- HyperEdge representations
data Hyper a b h where
    Simple   :: [Node] -> h -> Hyper () () h
    SDirect  :: [Node] -> [Node] -> h -> Hyper () () h
    --Hyper :: (Gr sub) => (sub a b, sub a b) -> Edge a b
    Conected :: Gr () a b -> h -> Hyper a b h
    CDirect  :: Gr () a b -> Gr () a b -> h -> Hyper a b h
 
-- HyperGraph representations
data HGr a b h = --HGr (Gr h a b) (HGraphRep h)
                 HGr (Gr h a b) (Gr () a h)
               | Shadow (Gr () a h)
  deriving (Show)

-- empty HyperGraph
hempty :: HGr a b h
hempty = HGr empty empty

-- add HyperEdge
{-
(+>>) :: (Hyper a b h) -> HGr a b h -> HGr a b h
(Conected g lab) +>> (HGr h1 h2) = let ns  = nodes g
                                       h2' = insBlock h2 ns
                                       h1' = unionGraphs g h1
                                   in HGr h1' h2'
-}

insBlock h2 ns = undefined

unionGraphs g h2 = let ns = labNodes g
                       es = labEdges g
                   in undefined --insEdges es (insNodes ns h)

{-
getSubGraph :: HGr a b h -> Gr a b
getSubGraph (HGr h1 h2) = let hcontext = getHContext' h1
                              scontext = toContext' hcontext
                          in Gr (IM.fromList context)   
-}

----------------------------------------------------------------------
-- UTILITIES
----------------------------------------------------------------------

----------------------------------------------------------------------
-- EXAMPLES
----------------------------------------------------------------------

-- enerate HyperEdges
h0 = Conected (mkGraph [(1,"a"), (2,"b")] [(1,2,"ab")])
              "hyper1"
              




















