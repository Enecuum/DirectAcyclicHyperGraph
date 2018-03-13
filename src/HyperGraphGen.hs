{-# LANGUAGE GADTs #-}

module HyperGraphGen where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM 

import HyperGraphTypes 

-- empty subgraph
empty :: Sub a b
empty = Sub IM.empty 

-- empty hypergraph
empty' :: HGr a b h
empty' = HGr (IM.empty, IM.empty) 

-- add a simple node 
(+>>) :: Node a -> Sub a b -> Sub a b 
(Node v l) +>> (Sub gr) = let em = IM.empty
                          in Sub $ IM.insert v (em, l, em) gr
infixr 8 +>>

-- add a simple edge 
(*>>) :: Edge b -> Sub a b -> Sub a b
(Edge v1 v2 l) *>> (Sub g) =
    case (IM.lookup v1 g, IM.lookup v2 g) of
      (_, Nothing) -> Sub g
      (Nothing, _) -> Sub g
      (Just (ps1, l1, ss1), Just (ps2, l2, ss2)) ->
        let ss1'  = IM.insertWith (++) v2 [l] ss1
            ps2'  = IM.insertWith (++) v1 [l] ps2
            cont1 = (ps1, l1, ss1')
            cont2 = (ps2', l2, ss2)
        in Sub $ IM.insert v1 cont2 (IM.insert v2 cont1 g)
infixr 7 *>>        

{-
-- add a hyperedge
(&>>) :: Sub a b -> HGr a b h -> HGr a b h
sub &>> (HGr (gr, bs)) =
    let size  = IM.size bs
        em    = IM.empty
        block = (em, em, , em, em)
        gr'   = sub >*< gr
        bs'   = IM.insert size block bs'
    in HGr (gr', bs')
 -}

-- add subgraph
(>*<) :: Sub a b -> HGraphRep a b h -> HGraphRep a b h
_ >*< _ = undefined
   
-- get nodes
nodes :: HGr a b h -> [Node a]
nodes (HGr (ns, _)) = undefined 

-- map of hypergraphs
mapHyper :: (a -> a1) -> HGr a b h -> HGr a1 b h
mapHyper = undefined

node1 = Node 1 "a"
node2 = Node 2 "b"

e1 = Edge 1 2 "ab"
e2 = Edge 1 2 "ab1"
h1 = Hyper sub2 "h1"

sub1 = node2 +>> (node1 +>> empty) :: Sub String String 
sub2 = node2 +>> (node1 +>> empty) :: Sub String String

instance (Show b) => Show (Edge b) where
    show (Edge v1 v2 l) = show (v1, v2, l)
    --show (Hyper g l)    = show (g, l)

