{-# LANGUAGE GADTs #-}

module HyperGraphTypes (
    Simple, Super, Vertex, 
    Node(..), Edge(..), HContext,
    Context, GraphRep, HGraphRep,
    Sub(..), HGr(..)
) where

import Data.IntMap (IntMap)

type Simple = String
type Super  = String

type Vertex = Int

data Node a where
    Node  :: Vertex -> Simple -> Node Simple
    Block :: Vertex -> Super -> Node Super

data Edge l where
    Edge  :: Vertex -> Vertex -> l -> Edge l
    Hyper :: Sub a b -> l -> Edge l

type HContext a b h = (IntMap [b], IntMap [b], a, IntMap [h], IntMap [h])

type Context a b = (IntMap [b], a, IntMap [b])

type GraphRep a b = IntMap (Context a b)

type HGraphRep a b h = IntMap (HContext a b h)

-- subgraph
data Sub a b = Sub (GraphRep a b)
  deriving (Show)

-- hypergraph
data HGr a b h = HGr (HGraphRep a b h, HGraphRep a b h)
  deriving (Show)