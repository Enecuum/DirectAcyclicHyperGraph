module HyperViz where

import HyperGraph 
import RandHyper

import qualified Data.GraphViz as G
import Data.GraphViz 
import Data.GraphViz.Types 
import Data.GraphViz.Types.Canonical
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete

--hyperToDot :: HGr a b h -> Dot ()
hyperToDot h = 
    let bs   = getDirBlocks h
        len1 = length $ hnodes h
        len2 = length $ bs 
        ns   = map (\v -> DotNode v [toLabel "", 
                                     color Blue, 
                                     shape Circle, 
                                     style filled, 
                                     Width 0.1, 
                                     Height 0.1])
                   [1..len1]
        hs   = map (\v -> DotNode v [toLabel "", 
                                     color Red, 
                                     shape Circle, 
                                     style filled, 
                                     Width 0.3, 
                                     Height 0.3])
                    [len1+1..len1+len2]
        es1  = concatMap (\(p,v,_,_) -> map (\x -> DotEdge x (v+len1)  [--toLabel (show x ++ show v),
                                                                        style tapered, 
                                                                        arrowTo noArrow]) 
                                            p)
                         bs
        es2  = concatMap (\(_,v,_,s) -> map (\x -> DotEdge (v+len1) x [--toLabel (show v ++ show x),
                                                                       style tapered, 
                                                                       arrowTo noArrow]) 
                                            s)
                         bs
    in  DotGraph False 
                 True 
                 Nothing 
                 (DotStmts [] [] (ns ++ hs) (es1 ++ es2))
   
test01 teg = genHyper 10 5 >>= \h -> runGraphviz (hyperToDot h) Png teg >> hprettyPrint h >> putStrLn (show $ hnodes h)

                    
-- runGraphviz gr Png "1.png"

