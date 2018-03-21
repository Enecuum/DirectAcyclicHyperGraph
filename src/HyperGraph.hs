{-# LANGUAGE GADTs, EmptyDataDecls#-}

module HyperGraph where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM 

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Data.Graph.Inductive.Graph --hiding (Edge)
import PatriciaTree

import System.Random
import Control.Monad (replicateM)

data Block h = Block
             | Statistic h
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
               | Shadow (IntMap [h], a, IntMap [h]) (HGraphRep h)
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

(Conected sub hl) +>> (HGr g h) = 
    let hedge = IS.fromList (nodes sub)
        hcont = SimpleHCont (hedge, hl)
        n     = IM.size h
        h'    = IM.insert n hcont h
        sub'  = insBlock hcont n sub
        g'    = mergeGraphs g sub'
    in HGr g' h'

(SDirect ns1 ns2 hl) +>> (Shadow g h) = undefined
 
{-
instance (Show a, Show b) => Show (Gr h a b) where
  showsPrec d g = showParen (d > 10) $
                    showString "mkGraph "
                    . shows (labNodes g)
                    . showString " "
                    . shows (labEdges g)
-}

----------------------------------------------------------------------
-- UTILITIES
----------------------------------------------------------------------

mkHContInit :: IntSet -> Block h -> HGraphRep h
mkHContInit hedge h = IM.insert 0 (SimpleHCont (hedge, h)) IM.empty 

insBlockInit :: Sub a b h -> Block h -> Sub a b h
insBlockInit (Gr cont) hl = Gr (IM.map (f hl) cont)
  where f hl (ps, ss, l, hps, hss) = 
          let hps' = IM.insert 0 hl hps
          in (ps, ss, l, hps', hss) 

mergeGraphs (Gr cont1) (Gr cont2) = Gr $ IM.unionWith u cont1 cont2
  where u (p, s, l, hp1, hs1) (_, _, _, hp2, hs2) =
          let hp = IM.union hp1 hp2
              hs = IM.union hs1 hs2
          in (p, s, l, hp, hs)

insBlock :: HContext h -> Int -> Sub a b h -> Sub a b h
insBlock (SimpleHCont (_, hl)) n (Gr cont) = Gr (IM.map (f n hl) cont)
  where f n hl (ps, ss, l, hps, hss) = 
          let hps' = IM.insert n hl hps 
          in (ps, ss, l, hps', hss)

getHCont (HGr (Gr cont) _) = cont 

getCont (Gr cont) = cont 

getGr (HGr g _) = g

----------------------------------------------------------------------
-- EXAMPLES
----------------------------------------------------------------------

-- generate HyperEdges
ns0  = [(1,"a"), (2,"b")]
es0  = [(1,2,"ab")]
sub0 = mkGraph ns0 es0 :: Sub String String (Block ())
h0   = Conected sub0 Block
hgr0 = h0 +>> hempty --mkHyperGraph h0

ns1   = [(1,"a"), (3,"c"), (4,"d")]
es1   = [(1,4,"cd")]
sub1  = mkGraph ns1 es1 :: Sub String String (Block ())
h1    = Conected sub1 Block
hgr1  = h1 +>> hgr0

exampleSimple = do 
    let es = [(v,()) | v <- [1..10]]
    ns <- replicateM 5 $ replicateM 3 $ randomRIO (1,10)
    let ns' = map IS.toList (map IS.fromList ns)
    return ns'






















