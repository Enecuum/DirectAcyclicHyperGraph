module HyperGraphExample where

import PatriciaHyperTree
import DirectAcyclicHyperGraph

nodes = [(1, "a"), (2, "b"), (3, "c"), (4, "d")] :: [LNode String]
hyper1 = LHyper ([1,2], [3], "abc") :: (LEdge String)
hyper2 = LHyper ([2,3], [4], "bcd") :: (LEdge String)
hypers = [hyper1, hyper2] :: [LEdge String]









 




