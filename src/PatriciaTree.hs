{-# LANGUAGE BangPatterns, CPP, ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif

-- |An efficient implementation of 'Data.Graph.Inductive.Graph.Graph'
-- using big-endian patricia tree (i.e. "Data.IntMap").
--
-- This module provides the following specialised functions to gain
-- more performance, using GHC's RULES pragma:
--
-- * 'Data.Graph.Inductive.Graph.insNode'
--
-- * 'Data.Graph.Inductive.Graph.insEdge'
--
-- * 'Data.Graph.Inductive.Graph.gmap'
--
-- * 'Data.Graph.Inductive.Graph.nmap'
--
-- * 'Data.Graph.Inductive.Graph.emap'

module PatriciaTree where

import Data.Graph.Inductive.Graph

import           Control.Applicative (liftA2)
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IM
import           Data.List           (foldl', sort)
import           Data.Maybe          (fromMaybe)

#if MIN_VERSION_containers (0,4,2)
import Control.DeepSeq (NFData(..))
#endif

#if MIN_VERSION_containers(0,5,0)
import qualified Data.IntMap.Strict as IMS
#else
import qualified Data.IntMap as IMS
#endif

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif

#if MIN_VERSION_base (4,8,0)
import Data.Bifunctor
#else
import Control.Arrow (second)
#endif

----------------------------------------------------------------------
-- GRAPH REPRESENTATION
----------------------------------------------------------------------

newtype Gr h a b = Gr (GraphRep a b h)
#if __GLASGOW_HASKELL__ >= 702
  deriving (Generic)
#endif

--enequm-- update Graph representations --enequm--
type GraphRep a b h = IntMap (Context' a b h)
--type GraphRep a b = IntMap (Context' a b)

--type HContext' a b h = (IntMap [b], IntMap [b], a, IntMap [h], IntMap [h])

--enequm-- update Context' to HyperContext' --enequm--
type Context' a b h = (IntMap [b], IntMap [b], a, IntMap h, IntMap h)

type UGr = Gr () ()

----------------------------------------------------------------------
-- CLASS INSTANCES
----------------------------------------------------------------------

--instance (Eq a, Ord b) => Eq (Gr a b) where
instance (Eq a, Ord b) => Eq (Gr h a b) where
  (Gr g1) == (Gr g2) = fmap sortAdj g1 == fmap sortAdj g2
    where
      sortAdj (p, s, n, hp, hs) = (fmap sort p,n,fmap sort s)

--instance (Show a, Show b) => Show (Gr a b) where
instance (Show a, Show b) => Show (Gr h a b) where
  showsPrec d g = showParen (d > 10) $
                    showString "mkGraph "
                    . shows (labNodes g)
                    . showString " "
                    . shows (labEdges g)

--instance (Read a, Read b) => Read (Gr a b) where
instance (Read a, Read b) => Read (Gr h a b) where
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("mkGraph", s) <- lex r
    (ns,t) <- reads s
    (es,u) <- reads t
    return (mkGraph ns es, u)

--instance Graph Gr where
instance Graph (Gr h) where
    empty           = Gr IM.empty

    isEmpty (Gr g)  = IM.null g

    match           = matchGr

    mkGraph vs es   = insEdges es
                      . Gr
                      . IM.fromList
                      . map (second (\l -> (IM.empty, IM.empty, l, IM.empty, IM.empty)))
                      $ vs

    labNodes (Gr g) = [ (node, label)
                            | (node, (_, _, label, _, _)) <- IM.toList g ]

    noNodes   (Gr g) = IM.size g

    nodeRange (Gr g) = fromMaybe (error "nodeRange of empty graph")
                       $ liftA2 (,) (ix (IM.minViewWithKey g))
                                    (ix (IM.maxViewWithKey g))
      where
        ix = fmap (fst . fst)

    labEdges (Gr g) = do --(node, (_, _, s)) <- IM.toList g
                         (node, (_, s, _, _, _)) <- IM.toList g
                         (next, labels)    <- IM.toList s
                         label             <- labels
                         return (node, next, label)

--instance DynGraph Gr where
instance DynGraph (Gr h) where
    (p, v, l, s) & (Gr g)
        = case IM.lookup v g of
            Nothing -> 
              let -- !g1 = IM.insert v (preds, l, succs) g 
                  -- !g1 = IM.insertWith f v (preds, succs, l, IM.empty, IM.empty) g
                  !g1 = IM.insert v (preds, succs, l, IM.empty, IM.empty) g
                  -- f (p, s, l, hp1, hs1) (_, _, _, hp2, hs2) = 
                  -- (p, s, l, IM.union hp1 hp2, IM.union hs1 hs2)
                  !(np, preds) = fromAdjCounting p
                  !(ns, succs) = fromAdjCounting s
                  !g2 = addSucc g1 v np preds
                  !g3 = addPred g2 v ns succs
              in Gr g3
            (Just (_, _, _, hp, hs)) ->
                let -- !g1 = IM.insert v (preds, l, succs) g 
                   -- !g1 = IM.insertWith f v (preds, succs, l, hp, hs) g 
                   !g1 = IM.insert v (preds, succs, l, hp, hs) g
                  -- f (p, s, l, hp1, hs1) (_, _, _, hp2, hs2) = 
                  -- (p, s, l, IM.union hp1 hp2, IM.union hs1 hs2)
                   !(np, preds) = fromAdjCounting p
                   !(ns, succs) = fromAdjCounting s
                   !g2 = addSucc g1 v np preds
                   !g3 = addPred g2 v ns succs
                in Gr g3
  
--enequm-- merge HyperContext  --enequm--
{-
(*&*) :: (v, Context') -> Gr h a b -> Gr h a b
(p, s, l, hp, hs) *&*  (Gr g) = 
  let !g1 = IM.insertWith f v (preds, succs, l, hp, hs) g 
      f (p, s, l, hp1, hs1) (_, _, _, hp2, hs2) = 
        (p, s, l, IM.union hp1 hp2, IM.union hs1 hs2) 
      !(np, preds) = fromAdjCounting p
      !(ns, succs) = fromAdjCounting s
      !g2 = addSucc g1 v np preds
      !g3 = addPred g2 v ns succs
  in Gr g3
-}

#if MIN_VERSION_containers (0,4,2)
--instance (NFData a, NFData b) => NFData (Gr a b) where
instance (NFData h, NFData a, NFData b) => NFData (Gr h a b) where
  rnf (Gr g) = rnf g
#endif

#if MIN_VERSION_base (4,8,0)
--instance Bifunctor Gr where
instance Bifunctor (Gr h) where
  bimap = fastNEMap

  first = fastNMap

  second = fastEMap
#endif

--matchGr :: Node -> Gr a b -> Decomp Gr a b
matchGr :: Node -> Gr h a b -> Decomp (Gr h) a b
matchGr node (Gr g)
    = case IM.lookup node g of
        Nothing
            -> (Nothing, Gr g)

        Just (p, s, label, hs, hp)
            -> let !g1 = IM.delete node g
                   !p' = IM.delete node p
                   !s' = IM.delete node s
                   !g2 = clearPred g1 node s'
                   !g3 = clearSucc g2 node p'
               in (Just (toAdj p', node, label, toAdj s), Gr g3)

----------------------------------------------------------------------
-- OVERRIDING FUNCTIONS
----------------------------------------------------------------------

{-# RULES
      "insNode/Data.Graph.Inductive.PatriciaTree"  insNode = fastInsNode
  #-}
--fastInsNode :: LNode a -> Gr a b -> Gr a b
fastInsNode :: LNode a -> Gr h a b -> Gr h a b
fastInsNode (v, l) (Gr g) = g' `seq` Gr g'
  where
    --g' = IM.insert v (IM.empty, l, IM.empty) g
    g' = IM.insert v (IM.empty, IM.empty, l, IM.empty, IM.empty) g

{-# RULES
      "insEdge/Data.Graph.Inductive.PatriciaTree"  insEdge = fastInsEdge
  #-}
--fastInsEdge :: LEdge b -> Gr a b -> Gr a b
fastInsEdge :: LEdge b -> Gr h a b -> Gr h a b
fastInsEdge (v, w, l) (Gr g) = g2 `seq` Gr g2
  where
    g1 = IM.adjust addS' v g
    g2 = IM.adjust addP' w g1

    --addS' (ps, l', ss) = (ps, l', IM.insertWith addLists w [l] ss)
    addS' (ps, ss, l', hss, hps) = (ps, IM.insertWith addLists w [l] ss, l', hss, hps)
    --addP' (ps, l', ss) = (IM.insertWith addLists v [l] ps, l', ss)
    addP' (ps, ss, l', hss, hps) = (IM.insertWith addLists v [l] ps, ss, l', hss, hps)

{-# RULES
      "gmap/Data.Graph.Inductive.PatriciaTree"  gmap = fastGMap
  #-}
fastGMap :: forall a b c d h. (Context a b -> Context c d) -> Gr h a b -> Gr h c d
fastGMap f (Gr g) = Gr (IM.mapWithKey f' g)
  where
    --f' :: Node -> Context' a b -> Context' c d
    f' :: Node -> Context' a b h -> Context' c d h
    f' = ((fromContext . f) .) . toContext
    --f' = ((fromContext . f) .) . toContext

{-# RULES
      "nmap/Data.Graph.Inductive.PatriciaTree"  nmap = fastNMap
  #-}
fastNMap :: forall a b c h. (a -> c) -> Gr h a b -> Gr h c b
fastNMap f (Gr g) = Gr (IM.map f' g)
  where
    --f' :: Context' a b -> Context' c b
    f' :: Context' a b h -> Context' c b h
    f' (ps, ss, a, hss, hps) = (ps, ss, f a, hss, hps)
    --f' (ps, a, ss) = (ps, f a, ss)

{-# RULES
      "emap/Data.Graph.Inductive.PatriciaTree"  emap = fastEMap
  #-}
fastEMap :: forall a b c h. (b -> c) -> Gr h a b -> Gr h a c
fastEMap f (Gr g) = Gr (IM.map f' g)
  where
    --f' :: Context' a b -> Context' a c
    f' :: Context' a b h -> Context' a c h
    --f' (ps, a, ss) = (IM.map (map f) ps, a, IM.map (map f) ss)
    f' (ps, ss, a, hss, hps) = (IM.map (map f) ps, IM.map (map f) ss, a, hss, hps)

{-# RULES
      "nemap/Data.Graph.Inductive.PatriciaTree"  nemap = fastNEMap
  #-}
fastNEMap :: forall a b c d h. (a -> c) -> (b -> d) -> Gr h a b -> Gr h c d 
fastNEMap fn fe (Gr g) = Gr (IM.map f g)
  where
    --f :: Context' a b h -> Context' c d h
    f :: Context' a b h -> Context' c d h
    f (ps, ss, a, hss, pss) = (IM.map (map fe) ps, IM.map (map fe) ss, fn a, hss, pss)
    --f (ps, a, ss) = (IM.map (map fe) ps, fn a, IM.map (map fe) ss)

----------------------------------------------------------------------
-- UTILITIES
----------------------------------------------------------------------

toAdj :: IntMap [b] -> Adj b
toAdj = concatMap expand . IM.toList
  where
    expand (n,ls) = map (flip (,) n) ls

fromAdj :: Adj b -> IntMap [b]
fromAdj = IM.fromListWith addLists . map (second (:[]) . swap)

data FromListCounting a = FromListCounting !Int !(IntMap a)
  deriving (Eq, Show, Read)

getFromListCounting :: FromListCounting a -> (Int, IntMap a)
getFromListCounting (FromListCounting i m) = (i, m)
{-# INLINE getFromListCounting #-}

fromListWithKeyCounting :: (Int -> a -> a -> a) -> [(Int, a)] -> (Int, IntMap a)
fromListWithKeyCounting f = getFromListCounting . foldl' ins (FromListCounting 0 IM.empty)
  where
    ins (FromListCounting i t) (k,x) = FromListCounting (i + 1) (IM.insertWithKey f k x t)
{-# INLINE fromListWithKeyCounting #-}

fromListWithCounting :: (a -> a -> a) -> [(Int, a)] -> (Int, IntMap a)
fromListWithCounting f = fromListWithKeyCounting (\_ x y -> f x y)
{-# INLINE fromListWithCounting #-}

fromAdjCounting :: Adj b -> (Int, IntMap [b])
fromAdjCounting = fromListWithCounting addLists . map (second (:[]) . swap)

-- We use differenceWith to modify a graph more than bulkThreshold times,
-- and repeated insertWith otherwise.
bulkThreshold :: Int
bulkThreshold = 5

--toContext :: Node -> Context' a b -> Context a b
toContext :: Node -> Context' a b h -> Context a b
toContext v (ps, ss, a, _, _) = (toAdj ps, v, a, toAdj ss)
--toContext v (ps, a, ss) = (toAdj ps, v, a, toAdj ss)

--fromContext :: Context a b -> Context' a b
fromContext :: Context a b -> Context' a b h
fromContext (ps, _, a, ss) = let em  = IM.empty
                                 ps' = fromAdj ps
                                 ss' = fromAdj ss
                             in (ps', ss', a, em, em)
--fromContext (ps, _, a, ss) = (fromAdj ps, a, fromAdj ss)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

-- A version of @++@ where order isn't important, so @xs ++ [x]@
-- becomes @x:xs@.  Used when we have to have a function of type @[a]
-- -> [a] -> [a]@ but one of the lists is just going to be a single
-- element (and it isn't possible to tell which).
addLists :: [a] -> [a] -> [a]
addLists [a] as  = a : as
addLists as  [a] = a : as
addLists xs  ys  = xs ++ ys

addSucc :: forall a b h . GraphRep a b h -> Node -> Int -> IM.IntMap [b] -> GraphRep a b h
addSucc g0 v numAdd xs
  | numAdd < bulkThreshold = foldlWithKey' go g0 xs
  where
    --go :: GraphRep a b -> Node -> [b] -> GraphRep a b
    go :: GraphRep a b h -> Node -> [b] -> GraphRep a b h
    go g p l = IMS.adjust f p g
      where f (ps, ss, l', hss, hps) = let !ss' = IM.insertWith addLists v l ss
                                       in (ps, ss', l', hss, hps)
    --go g p l = IMS.adjust f p g
    --  where f (ps, l', ss) = let !ss' = IM.insertWith addLists v l ss
    --                         in (ps, l', ss')
addSucc g v _ xs = IMS.differenceWith go g xs
  where
    --go :: Context' a b -> [b] -> Maybe (Context' a b)
    go :: Context' a b h -> [b] -> Maybe (Context' a b h)
    go (ps, ss, l', hss, hps) l = let !ss' = IM.insertWith addLists v l ss
                                  in Just (ps, ss', l', hss, hps)
   -- go (ps, l', ss) l = let !ss' = IM.insertWith addLists v l ss
   --                     in Just (ps, l', ss')

foldlWithKey' :: (a -> IM.Key -> b -> a) -> a -> IntMap b -> a
foldlWithKey' =
#if MIN_VERSION_containers (0,4,2)
  IM.foldlWithKey'
#else
  IM.foldWithKey . adjustFunc
  where
    adjustFunc f k b a = f a k b
#endif

addPred :: forall a b h . GraphRep a b h -> Node -> Int -> IM.IntMap [b] -> GraphRep a b h
addPred g0 v numAdd xs
  | numAdd < bulkThreshold = foldlWithKey' go g0 xs
  where
    --go :: GraphRep a b -> Node -> [b] -> GraphRep a b
    go :: GraphRep a b h -> Node -> [b] -> GraphRep a b h
    go g p l = IMS.adjust f p g
      where f (ps, ss, l', hps, hss) = let !ps' = IM.insertWith addLists v l ps
                                       in (ps', ss, l', hps, hss)
    --go g p l = IMS.adjust f p g
    --  where f (ps, l', ss) = let !ps' = IM.insertWith addLists v l ps
    --                         in (ps', l', ss)
addPred g v _ xs = IMS.differenceWith go g xs
  where
    --go :: Context' a b -> [b] -> Maybe (Context' a b)
    go :: Context' a b h -> [b] -> Maybe (Context' a b h)
    go (ps, ss, l', hps, hss) l = let !ps' = IM.insertWith addLists v l ps
                                  in Just (ps', ss, l', hps, hss)
    --go (ps, l', ss) l = let !ps' = IM.insertWith addLists v l ps
    --                    in Just (ps', l', ss)

clearSucc :: forall a b x h . GraphRep a b h -> Node -> IM.IntMap x -> GraphRep a b h
clearSucc g v = IMS.differenceWith go g
  where
    --go :: Context' a b -> x -> Maybe (Context' a b)
    go :: Context' a b h -> x -> Maybe (Context' a b h)
    go (ps, ss, l, hps, hss) _ = let !ss' = IM.delete v ss
                                 in Just (ps, ss', l, hps, hss)
    --go (ps, l, ss) _ = let !ss' = IM.delete v ss
    --                   in Just (ps, l, ss')

clearPred :: forall a b x h . GraphRep a b h -> Node -> IM.IntMap x -> GraphRep a b h
clearPred g v = IMS.differenceWith go g
  where
    --go :: Context' a b -> x -> Maybe (Context' a b)
    go :: Context' a b h -> x -> Maybe (Context' a b h)
    go (ps, ss, l, hps, hss) _ = let !ps' = IM.delete v ps
                                 in Just (ps', ss, l, hps, hss)
    --go (ps, l, ss) _ = let !ps' = IM.delete v ps
    --                   in Just (ps', l, ss)
