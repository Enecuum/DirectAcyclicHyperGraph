{-# LANGUAGE GADTs, EmptyDataDecls #-}

module HyperGraph where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM 

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import DirectHyperGraph 
import PatriciaTree

import System.Random
import Control.Monad (replicateM)

data Block h = Block
             | Label h
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
--data HNode a where
--    HNode :: Node -> Node -> Simple -> HNode Simple
--      Block :: Node -> Node -> Super  -> HNode Super

type Sub a b h = Gr (Block h) a b 

-- HyperEdge representations
data Hyper a b h where
    Simple   :: [Node] -> Block h -> Hyper () () h
    SDirect  :: [Node] -> [Node] -> Block h -> Hyper () () h
    --Hyper :: (Gr sub) => (sub a b, sub a b) -> Edge a b
    Conected :: Sub a b h -> Block h -> Hyper a b h
    CDirect  :: Sub a b h -> Sub a b h -> Block h -> Hyper a b h

data HGr a b h = --HGr (Gr h a b) (HGraphRep h)
                 HGr (Sub a b h) (HGraphRep h)
               | HyperGraph (UGr h) (HGraphRep h)
  deriving (Show)

-- empty HyperGraph
hempty :: HGr a b h
hempty = HGr empty IM.empty

-- insert HyperEdge
(+>>) :: Hyper a b h -> HGr a b h -> HGr a b h

(Simple ns hl) +>> (HGr g h) = 
    let hedge = IS.fromList ns
        hcont = SimpleHCont (hedge, hl)
        sub   = mkUGraph ns []
        n     = IM.size h + 1
        h'    = IM.insert n hcont h
        sub'  = insBlock hcont n sub
        g1    = mergeGraphs g sub'
    in HGr g1 h'

(Conected sub hl) +>> (HGr g h) = 
    let hedge = IS.fromList (nodes sub)
        hcont = SimpleHCont (hedge, hl)
        n     = IM.size h + 1
        h'    = IM.insert n hcont h
        sub'  = insBlock hcont n sub
        g1    = --insEdges (labEdges sub) $ insNodes (labNodes sub) g 
                mergeGraphs g sub'
        --g2    = insBlock hcont n g1
    in HGr g1 h'

(CDirect sub1 sub2 hl) +>> (HGr g h) = 
    let hedge = (IS.fromList $ nodes sub1, IS.fromList $ nodes sub2)
        hcont = DirectHCont (fst hedge, hl, snd hedge)
        n     = IM.size h + 1
        h'    = IM.insert n hcont h
        sub1' = insBlockPreds hcont n sub1
        sub2' = insBlockSuccs hcont n sub2
        g1    = mergeGraphs (mergeGraphs g sub1') sub2'
    in HGr g1 h' 


getBlocks :: HGr a b h -> [(Int, [Int], Block h)]
getBlocks (HGr _ h) = map f (IM.toList h)
    where f (n, (SimpleHCont (he, l))) = (n, (IS.toList he), l)

getDirBlocks :: HGr a b h -> [([Int], Int, Block h, [Int])]
getDirBlocks (HGr _ h) = map f (IM.toList h)
    where f (n, (DirectHCont (hp,l,hs))) = (IS.toList hp, n, l, IS.toList hs)

-- pretty printing of hypergraph
hprettify :: (Show a, Show b, Show h) => HGr a b h -> String
hprettify hg = foldr showsContext id (getDirBlocks hg) ""
  where
    showsContext (hp,n,l,hs) sg = ("Hyper "++) . shows n  
                                  . showString " -> " . shows hp
                                  . showString " "    . shows hs
                                  . ('\n':) . sg

hprettyPrint :: (Show a, Show b, Show h) => HGr a b h -> IO ()
hprettyPrint = putStr . hprettify

hsize :: HGr a b h -> Int
hsize (HGr g h) = IM.size h

hnodes :: HGr a b h -> [Node]
hnodes (HGr g _) = nodes g

{-
mkHyper :: [LNode a] -> HGr a b h
mkHyper ns = let sub = mkGraph ns [] -- :: Sub a b h
             in HGr sub IM.empty
-}

mkHyperGraph :: [Node] -> [([Node], h)] -> HGr () () h
mkHyperGraph ns hes = let sub  = mkUGraph ns []
                          init = HGr sub IM.empty
                          hs   = map (\(ns,hl) -> Simple ns (Label hl)) hes
                      in foldl (flip (+>>)) init hs

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

insBlockPreds :: HContext h -> Int -> Sub a b h -> Sub a b h
insBlockPreds (DirectHCont (_, hl, _)) n (Gr cont) = Gr (IM.map (f n hl) cont)
  where f n hl (ps, ss, l, hps, hss) = 
          let hps' = IM.insert n hl hps 
          in (ps, ss, l, hps', hss)

insBlockSuccs :: HContext h -> Int -> Sub a b h -> Sub a b h
insBlockSuccs (DirectHCont (_, hl, _)) n (Gr cont) = Gr (IM.map (f n hl) cont)
  where f n hl (ps, ss, l, hps, hss) = 
          let hss' = IM.insert n hl hss 
          in (ps, ss, l, hps, hss')

getHCont (HGr (Gr cont) _) = cont 

getHyperCont (HGr _ h) = h

getCont (Gr cont) = cont 

getGr (HGr g _) = g

----------------------------------------------------------------------
-- Test
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

g1 = getGr hgr1
cont1 = getCont g1

g2 = insNode (3,"c") g1
cont2 = getCont g2

g3 = insEdge (2,1,"ba") g1
cont3 = getCont g3

-- not orinted graph
nodes01 = [(x,()) | x <- [1..3]]
nodes02 = [(x,()) | x <- [4..8]]
sub01 = mkGraph nodes01 [] :: Sub () () ()
sub02 = mkGraph nodes02 [] :: Sub () () ()
he01 = Conected sub01 Block
he02 = Conected sub01 Block
hypergraph01 = he02 +>> (he01 +>> hempty)

-- oriented graph
nodes11 = [(x,x) | x <- [1..4]]
nodes12 = [(x,x) | x <- [5..7]]
sub11 = mkGraph nodes11 [] :: Sub Int Int ()
sub12 = mkGraph nodes12 [] :: Sub Int Int ()
he11 = CDirect sub11 sub12 Block
hypergraph11 = he11 +>> hempty

nodes21 = [(x,x) | x <- [2..5]]
nodes22 = [(x,x) | x <- [6..9]]
sub21 = mkGraph nodes21 [] :: Sub Int Int ()
sub22 = mkGraph nodes22 [] :: Sub Int Int ()
he21 = CDirect sub21 sub22 Block
hypergraph21 = he21 +>> (he11 +>> hempty)









