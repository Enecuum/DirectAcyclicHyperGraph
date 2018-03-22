{-# LANGUAGE GADTs, EmptyDataDecls#-}

module HyperGraph where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM 

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import DirectHyperGraph hiding (Edge)
import PatriciaTree

import System.Random
import Control.Monad (replicateM)

data Block h = Block
             | Statistic { block :: Block h, count :: Int, time :: Double, amount :: Int }
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

data HGr a b h = --HGr (Gr h a b) (HGraphRep h)
                 HGr (Sub a b h) (HGraphRep h)
               | Shadow (IntMap [h], a, IntMap [h]) (HGraphRep h)
  deriving (Show)

-- empty HyperGraph
hempty :: HGr a b h
hempty = HGr empty IM.empty

-- add HyperEdge
(+>>) :: Hyper a b h -> HGr a b h -> HGr a b h

(Conected sub hl) +>> (HGr g h) = 
    let hedge = IS.fromList (nodes sub)
        hcont = SimpleHCont (hedge, hl)
        n     = IM.size h 
        h'    = IM.insert n hcont h
        sub'  = insBlock hcont n sub
        g1    = --insEdges (labEdges sub) $ insNodes (labNodes sub) g 
                mergeGraphs g sub'
        --g2    = insBlock hcont n g1
    in HGr g1 h'

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
-- HELPERS
----------------------------------------------------------------------

mkHContInit :: IntSet -> Block h -> HGraphRep h
mkHContInit hedge h = IM.insert 0 (SimpleHCont (hedge, h)) IM.empty 

insBlockInit :: Sub a b h -> Block h -> Sub a b h
insBlockInit (Gr cont) hl = Gr (IM.map (f hl) cont)
  where f hl (ps, ss, l, hps, hss) = 
          let hps' = IM.insert 0 hl hps
          in (ps, ss, l, hps', hss)  

mergeGraphs (Gr gr') gr = IM.foldlWithKey' insCont gr gr'

insCont :: Gr h a b  -> Node -> Context' a b h -> Gr h a b 
insCont g v (p1,s1,l,hp1,hs1) = 
    case matchHGr v g of
        (Nothing,_) -> (toAdj p1, toAdj s1, v, l, hp1, hs1) *& g    
        (Just (p2, s2, _, _, hp2, hs2), m) -> 
            let adj1 = p2 ++ toAdj p1
                adj2 = s2 ++ toAdj s1
                hp3  = IM.union hp1 hp2
                hs3  = IM.union hs1 hs2
            in (adj1, adj2, v, l, hp3, hs3) *& m


insBlock :: HContext h -> Int -> Sub a b h -> Sub a b h
insBlock (SimpleHCont (_, hl)) n (Gr cont) = Gr (IM.map (f n hl) cont)
  where f n hl (ps, ss, l, hps, hss) = 
          let hps' = IM.insert n hl hps 
          in (ps, ss, l, hps', hss)

getHCont (HGr (Gr cont) _) = cont 

getHyperCont (HGr _ h) = h

getCont (Gr cont) = cont 

getGr (HGr g _) = g

----------------------------------------------------------------------
-- EXAMPLES
----------------------------------------------------------------------

-- generate HyperEdges
ns1  = [(1,"a"), (2,"b")]
es1  = [(1,2,"h1")]
sub1 = mkGraph ns1 es1 :: Sub String String ()
h1   = Conected sub1 Block
hgr1 = h1 +>> hempty --mkHyperGraph h0

ns2   = [(1,"a"), (2,"b"), (3,"c")]
es2   = [(1,2,"h2"), (2,3,"h2")]
sub2  = mkGraph ns2 es2 :: Sub String String ()
h2    = Conected sub2 Block
hgr2  = h2 +>> hgr1

ns3  = [(4,"c"), (5,"d"), (6,"e")]
es3  = [(4,5,"h3"), (4,6,"h3")]
sub3 = mkGraph ns3 es3 :: Sub String String ()
h3   = Conected sub3 Block
hgr3 = h3 +>> hempty

exampleSimple = do 
    let es = [(v,()) | v <- [1..10]]
    ns <- replicateM 5 $ replicateM 3 $ randomRIO (1,10)
    let ns' = map IS.toList (map IS.fromList ns)
    return ns'

g1 = getGr hgr1
cont1 = getCont g1

g2 = insNode (3,"c") g1
cont2 = getCont g2

g3 = insEdge (2,1,"ba") g1
cont3 = getCont g3

ns = [(1,"a"), (2,"b"), (3,"c")]
es = [(1,2,"ac")]
gr = mkGraph ns3 es3 :: Gr () String String

blocksCont hgr = IM.map (\(_,_,_,hp,hs) -> (hp,hs)) (getHCont hgr)


--insCont' :: Gr h a b  -> Node -> Context' a b h -> Gr h a b 
insCont' g v (p,s,l,hp,hs) = 
    case match v g of
        (Nothing,_) -> (toAdj p, toAdj s)     
        (Just (p', _, _, s'), m) -> 
            let adj1 = p' ++ toAdj p
                adj2 = s' ++ toAdj s
            in (adj1, adj2)




















