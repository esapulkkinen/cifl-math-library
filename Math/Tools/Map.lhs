>{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, Arrows #-}
>{-# OPTIONS_GHC -fno-warn-orphans #-}
>module Math.Tools.Map where
>import Prelude hiding (sequence,mapM)
>import Control.Monad (zipWithM)
>import Data.Maybe (fromJust,isJust)
>import Control.Arrow
>import Data.Map (Map)
>import qualified Data.Map as Map
>import Math.Tools.Arrow
>import Math.Tools.Universe
>import Math.Tools.FunctorM

import Maybe (fromJust, isJust)

>instance Applicative (Map String) where
>   pure x = return x
>   f <*> x = f >>= \f' -> x >>= \x' -> return (f' x')

>instance Monad (Map String) where
>   return x = Map.singleton "_" x
>   m >>= f = joinMap (fmap f m)
>   fail msg = Map.empty

>instance (Ord a, Eq b, Universe a, Universe b) => Universe (Map a b) where
>   all_elements = all_maps all_elements all_elements

>all_maps :: (Ord k) => [k] -> [a] -> [Map k a]
>all_maps k x = concatMap (`all_total_maps` x) (all_subsets k)

all_total_maps gives all maps whose set of keys is exactly the given
set.

>all_total_maps :: (Ord k) => [k] -> [a] -> [Map k a]
>all_total_maps [] _ = [Map.empty]
>all_total_maps (c:cr) el = do r <- all_total_maps cr el
>                              e <- el
>                              return (Map.insert c e r)

>lookupMapM :: (Monad m, Show i, Ord i) => Map i a -> i -> m a
>lookupMapM m i | Just v <- Map.lookup i m = return v
>               | otherwise = fail $ "Cannot find item:" ++ show i ++ "\n"
>                                 ++ "Alternatives are:" ++ show (Map.keys m)


>unzip :: Map i (a,b) -> (Map i a, Map i b)
>unzip m = (fmap fst m, fmap snd m)

>unzip3 :: Map i (a,b,c) -> (Map i a, Map i b, Map i c)
>unzip3 m = (fmap take1 m, fmap take2 m, fmap take3 m)
>   where  take1 (a,_,_) = a
>          take2 (_,b,_) = b
>          take3 (_,_,c) = c

>foldM :: (Monad m) => (a -> b -> m b) -> m b -> Map i a -> m b
>foldM f x = Map.foldr (\a mb -> mb >>= f a) x

>foldKeyM :: (Monad m) => (i -> a -> b -> m b) -> m b -> Map i a -> m b
>foldKeyM f x = Map.foldrWithKey (\k a mb -> mb >>= f k a) x

>mapM :: (Monad m, Ord i) => (a -> m b) -> Map i a -> m (Map i b)
>mapM f m = sequence_map $ fmap f m

>sequence_ :: (Monad m, Ord i) => Map i (m a) -> m ()
>sequence_ m = sequence_map m >> return ()

>instance (Ord i) => FunctorM (Map i) where
>   mapMF = mapM

>sequence_map :: (Monad m,Ord i) => Map i (m a) -> m (Map i a)
>sequence_map = Map.foldrWithKey (\k act res -> do a <- act
>                                                  m <- res
>                                                  return (Map.insert k a m))
>                           (return Map.empty)

>sequenceByKey :: (Monad m, Ord i) => Map i (i -> m a) -> m (Map i a)
>sequenceByKey = Map.foldrWithKey (\k act res -> do a <- act k
>                                                   m <- res
>                                                   return (Map.insert k a m))
>                          (return Map.empty)

>joinMap :: Map String (Map String a) -> Map String a
>joinMap = Map.foldrWithKey (\ k m1 r -> Map.union (Map.mapKeys (\x -> k ++ ('.':x)) m1) r)
>                          Map.empty

>splitMapByKey :: (Ord s,Ord m) => Map (m,s) a -> (Map m (Map s a))
>splitMapByKey m = Map.mapKeysWith Map.union fst (Map.mapWithKey (\ (_,s) x -> Map.singleton s x) m)

>partitionMap :: (Ord k) => Map k a -> (a -> Either b c) -> (Map k b, Map k c)
>partitionMap m f = let (a,b) = Map.partition isLeft (Map.map f m)
>                    in (Map.map fromLeft a, Map.map fromRight b)
>    where fromLeft :: Either a b -> a
>          fromLeft  ~(Left x) = x
>          fromRight :: Either a b -> b
>          fromRight ~(Right y) = y
>          isLeft (Left _) = True
>          isLeft (Right _) = False

>intersectZipMapA :: (Ord i, ArrowChoice arr) =>
>                    arr (Map.Map i a, Map.Map i b) (Map.Map i (a,b))
>intersectZipMapA = intersectMapA returnA

intersectMapA intersects two maps in O(n log(n)) time.

>intersectMapA :: (Ord i, ArrowChoice arr) => arr (a,b) c
>              -> arr (Map.Map i a, Map.Map i b) (Map.Map i c)
>intersectMapA f = proc (m1,m2) -> do
>             let k1 = filter (`Map.member` m2) $ Map.keys m1
>             unifiedLst <- amap f -< zip (map (m1 Map.!) k1) (map (m2 Map.!) k1)
>             returnA -< Map.fromAscList $ zip k1 unifiedLst

zipWithMapM zips the maps together in such way that if the indices
match, it combines using the given function. The indices and values
that do not match in the maps are returned separately.

>zipWithMapM :: (Monad m, Ord i) => (a -> b -> m c) -> Map i a -> Map i b -> m (Map i c, Map i a, Map i b)
>zipWithMapM f x y = do ulst <- zipWithM mapper blst clst
>                       return (Map.fromAscList ulst,brest,crest)
>   where mapper (i,e) (j,e') | i == j = f e e' >>= (return . ((,) i))
>                             | otherwise = fail "zipWithMapM: Internal error"
>         blst = Map.toAscList b
>         clst = Map.toAscList c
>         b = x `Map.intersection` y
>         c = y `Map.intersection` x
>         brest = x `Map.difference` b
>         crest = y `Map.difference` c

>intersectMapKeyA :: (Ord i, ArrowChoice arr) => arr (i,(a,b)) c
>                 -> arr (Map.Map i a, Map.Map i b) (Map.Map i c)
>intersectMapKeyA f = proc (m1,m2) -> do
>             let k1 = filter (`Map.member` m2) $ Map.keys m1
>             unifiedLst <- amap f -< zip k1 (zip (map (m1 Map.!) k1) (map (m2 Map.!) k1))
>             returnA -< Map.fromAscList $ zip k1 unifiedLst

>instance (ArrowChoice arr, Ord i) => FunctorArrow (Map.Map i) arr where
>   amap f = proc m1 -> do
>            let alst = Map.toAscList m1
>            alst' <- amap (second f) -< alst
>            returnA -< Map.fromAscList alst'

>mapMapWithKey :: (ArrowChoice arr, Eq i) => arr (i,e) (i,f)
>                                         -> arr (Map.Map i e) (Map.Map i f)
>mapMapWithKey f = proc m1 -> do
>                  let alst = Map.toAscList m1
>                  alst' <- amap f -< alst
>                  returnA -< Map.fromAscList alst'

>unionMapA :: (Ord i, ArrowChoice arr) => arr (a,a) a
>          -> arr (Map.Map i a, Map.Map i a) (Map.Map i a)
>unionMapA f = proc (m1,m2) -> do
>              unifiedMap <- intersectMapA f -< (m1,m2)
>              returnA -< (m1 Map.\\ m2) `Map.union` (m2 Map.\\ m1) `Map.union` unifiedMap

>sequenceMap :: (Ord i) => Map i (IO b) -> IO (Map i b)
>sequenceMap = Map.foldrWithKey (\ k iob iom -> do b <- iob
>                                                  r <- iom
>                                                  return (Map.insert k b r))
>                              (return (Map.empty))

>sequenceMapA :: (Arrow arr, Ord i) => Map i (arr a b) -> arr a (Map i b)
>sequenceMapA = Map.foldrWithKey (\ k ab ar -> proc x' -> do
>                                                b <- ab -< x'
>                                                r <- ar -< x'
>                                                returnA -< (Map.insert k b r))
>                                         (proc _ -> returnA -< Map.empty)

>sequenceMapWithKeyA :: (Arrow arr, Ord i) => Map i (arr (a,i) b) -> arr a (Map i b)
>sequenceMapWithKeyA = Map.foldrWithKey (\ k ab ar -> proc x' -> do
>                                               b <- ab -< (x',k)
>                                               r <- ar -< x'
>                                               returnA -< (Map.insert k b r))
>                                         (proc _ -> returnA -< Map.empty)

>foldMapA :: (ArrowApply arr) => arr (a,b) b -> arr (Map.Map i a, b) b
>foldMapA f = proc (m,r0) -> Map.foldr (\val r -> r >>> (proc j -> f -< (val,j))) returnA m -<< r0 

>foldMapKeyA :: (ArrowApply arr) => arr (i,a,b) b -> arr (Map.Map i a, b) b
>foldMapKeyA f = proc (m,r0) -> Map.foldrWithKey (\k val r -> r >>> (proc j -> f -< (k,val,j))) returnA m -<< r0

>composeMapA :: (Arrow arr, Ord i, Ord a) => arr (Map.Map i a, Map.Map a b) (Map.Map i b)
>composeMapA = proc (m1, m2) -> do
>              returnA -< Map.map fromJust $
>                          Map.filter isJust $
>                            Map.map (\k -> Map.lookup k m2) m1
>                                                            
