{-# LANGUAGE BangPatterns, CPP, ScopedTypeVariables #-}

module HyperGraph where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM 
import Data.Bifunctor (second)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.IntMap.Strict as IMS
import Data.List (foldl', sort)


type Node         = Int
type LNode a      = (Node, a)

data LEdge b      = LEdge (Node, Node, b)
                  | LHyper ([Node], [Node], b)

--type LEdge b = (Node, Node, b)

type Adj b        = [(b, Node)]
type Context a b  = (Adj b, Node, a, Adj b)

newtype Gr a b = Gr (GraphRep a b)
  deriving (Show)

type GraphRep a b = IntMap (Context' a b)
type Context' a b = (IntMap [b], a, IntMap [b])

type Decomp g a b = (MContext a b,g a b)

type MContext a b = Maybe (Context a b)

type UGr = Gr () ()

(&) :: Context a b -> Gr a b -> Gr a b

(p, v, l, s) & (Gr g)
  = let !g1 = IM.insert v (preds, l, succs) g
        !(np, preds) = fromAdjCounting p
        !(ns, succs) = fromAdjCounting s
        !g2 = addSucc g1 v np preds
        !g3 = addPred g2 v ns succs
    in Gr g3

match = matchGr

matchGr :: Node -> Gr a b -> Decomp Gr a b
matchGr node (Gr g)
    = case IM.lookup node g of
        Nothing
            -> (Nothing, Gr g)

        Just (p, label, s)
            -> let !g1 = IM.delete node g
                   !p' = IM.delete node p
                   !s' = IM.delete node s
                   !g2 = clearPred g1 node s'
                   !g3 = clearSucc g2 node p'
               in (Just (toAdj p', node, label, toAdj s), Gr g3)

mkGraph vs es = insEdges es
                . Gr
                . IM.fromList
                . map (second (\l -> (IM.empty,l,IM.empty)))
                $ vs

insEdge :: LEdge b -> Gr a b -> Gr a b
insEdge (LEdge (v,w,l)) g = (pr,v,la,(l,w):su) & g'
  where
    (mcxt,g') = match v g
    (pr,_,la,su) = fromMaybe
                     (error ("insEdge: cannot add edge from non-existent vertex " ++ show v))
                     mcxt

insEdges :: [LEdge b] -> Gr a b -> Gr a b
insEdges es g = foldl' (flip insEdge) g es

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

toContext :: Node -> Context' a b -> Context a b
toContext v (ps, a, ss) = (toAdj ps, v, a, toAdj ss)

fromContext :: Context a b -> Context' a b
fromContext (ps, _, a, ss) = (fromAdj ps, a, fromAdj ss)

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

addSucc :: forall a b . GraphRep a b -> Node -> Int -> IM.IntMap [b] -> GraphRep a b
addSucc g0 v numAdd xs
  | numAdd < bulkThreshold = foldlWithKey' go g0 xs
  where
    go :: GraphRep a b -> Node -> [b] -> GraphRep a b
    go g p l = IMS.adjust f p g
      where f (ps, l', ss) = let !ss' = IM.insertWith addLists v l ss
                             in (ps, l', ss')
addSucc g v _ xs = IMS.differenceWith go g xs
  where
    go :: Context' a b -> [b] -> Maybe (Context' a b)
    go (ps, l', ss) l = let !ss' = IM.insertWith addLists v l ss
                        in Just (ps, l', ss')

foldlWithKey' :: (a -> IM.Key -> b -> a) -> a -> IntMap b -> a
foldlWithKey' =
#if MIN_VERSION_containers (0,4,2)
  IM.foldlWithKey'
#else
  IM.foldWithKey . adjustFunc
  where
    adjustFunc f k b a = f a k b
#endif

addPred :: forall a b . GraphRep a b -> Node -> Int -> IM.IntMap [b] -> GraphRep a b
addPred g0 v numAdd xs
  | numAdd < bulkThreshold = foldlWithKey' go g0 xs
  where
    go :: GraphRep a b -> Node -> [b] -> GraphRep a b
    go g p l = IMS.adjust f p g
      where f (ps, l', ss) = let !ps' = IM.insertWith addLists v l ps
                             in (ps', l', ss)
addPred g v _ xs = IMS.differenceWith go g xs
  where
    go :: Context' a b -> [b] -> Maybe (Context' a b)
    go (ps, l', ss) l = let !ps' = IM.insertWith addLists v l ps
                        in Just (ps', l', ss)

clearSucc :: forall a b x . GraphRep a b -> Node -> IM.IntMap x -> GraphRep a b
clearSucc g v = IMS.differenceWith go g
  where
    go :: Context' a b -> x -> Maybe (Context' a b)
    go (ps, l, ss) _ = let !ss' = IM.delete v ss
                       in Just (ps, l, ss')

clearPred :: forall a b x . GraphRep a b -> Node -> IM.IntMap x -> GraphRep a b
clearPred g v = IMS.differenceWith go g
  where
    go :: Context' a b -> x -> Maybe (Context' a b)
    go (ps, l, ss) _ = let !ps' = IM.delete v ps
                       in Just (ps', l, ss)

ns = [(1, "a"), (2, "b"), (3, "c")] 

e1 = LEdge (1,2,"ab")
e2 = LEdge (1,3,"ac")
e3 = LEdge (2,3,"bc")

es = [e1, e2]

gr = mkGraph ns es

inner'gr = let (Gr inner) = gr in inner

--instance (Show a, Show b) => Show (Gr a b) where
--  show (Gr xs) = show xs











                      






 









