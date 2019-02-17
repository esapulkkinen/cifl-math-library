>{-# LANGUAGE Safe, TypeFamilies, ExistentialQuantification, MultiParamTypeClasses,
>    FunctionalDependencies, FlexibleInstances, Rank2Types, FlexibleContexts, UndecidableInstances #-}
>module Math.Tools.FixedPoint where
>import Math.Tools.Visitor
>import Math.Tools.Universe

>type Coalgebra f a = a -> f a

>-- | These are standard definitions of final coalgebra and initial
>--  algebra from category theory.
>class (Functor f) => FinalCoalgebra f a | a -> f where
>      unDestroy :: f a -> a
>      unfold_gen :: (b -> f b) -> b -> a -- a.k.a. "count_iterations"
>      unfold_gen psi = unDestroy . fmap (unfold_gen psi) . psi

>type Algebra f a = f a -> a

>class (Functor f) => InitialAlgebra f a | a -> f where
>      unCreate :: a -> f a
>      fold_gen :: (f b -> b) -> a -> b
>      fold_gen phi = phi . fmap (fold_gen phi) . unCreate


>-- | PRIMITIVES FOR RECURSIVE DATA STRUCTURES
>data Rec f = In ! (f (Rec f))     -- recursion (least fixed point)
>data CoRec f = CoIn (f (CoRec f)) -- corecursion (greatest fixed point)

>unIn :: Rec f -> f (Rec f)
>unIn ~(In x) = x

>unStrictIn :: CoRec f -> f (CoRec f)
>unStrictIn (CoIn x) = x

>-- | fold over arbitrary functor, using standard definition.
>fold :: (Functor f) => (f a -> a) -> Rec f -> a
>fold phi = phi . fmap (fold phi) . unIn

>-- | unfold over arbitrary functor, using standard definition.
>unfold :: (Functor f) => (a -> f a) -> a -> Rec f -- a.k.a. "count_iterations"
>unfold psi = In . fmap (unfold psi) . psi

>transform :: Functor f => (b -> f b) -> (f a -> a) -> b -> a
>transform divide combine = fold combine . unfold divide

>-- | Note the builder and visitor for Rec f allows every level of the
>-- recursion to be done by different function. This allows distinctions
>-- by level to be implemented easily as nested RecFold data structures.
>-- Otherwise this distinction would have to be included in the function
>-- itself (using fmap).
>instance (Functor f) => Visitor (Rec f) where
>   data Fold (Rec f) a = forall b.RecFold (f b -> a) (Fold (Rec f) b)
>   visit (RecFold f z) (In x) = f (fmap (visit z) x)

>instance (Functor f) => ComposableVisitor (Rec f) where
>   embed f = RecFold (f . In) (embed id)

>instance (Functor f) => Builder (Rec f) where
>   data Unfold (Rec f) a = RecUnfold (a -> f a)
>   build z@(RecUnfold f) x = In $ fmap (build z) $ f x

>instance (Universe (f (Rec f))) => Universe (Rec f) where
>   all_elements = fmap In all_elements


>fold_level :: Fold (Rec f) a -> Fold (Rec f) (f a)
>fold_level = RecFold id

>recfold_twice :: (f b -> a) -> (f c -> b) -> Fold (Rec f) c -> Fold (Rec f) a
>recfold_twice f g = RecFold f . RecFold g

>recfold :: (f a -> a) -> Fold (Rec f) a
>recfold f = let x = RecFold f x in x

>recfold2 :: (f b -> a) -> (f a -> b) -> Fold (Rec f) a
>recfold2 f g = let x = RecFold f (RecFold g x) in x

>recfold_lst :: [f a -> a] -> Fold (Rec f) a
>recfold_lst lst = let x = foldr RecFold x lst in x

>fold_coerce :: (forall a. f a -> g a) -> Fold (Rec f) (Rec g)
>fold_coerce alfa = recfold (In . alfa)


>-- | strict foldl
>foldl' :: (a -> b -> a) -> a -> [b] -> a
>foldl' _ a [] = a
>foldl' f a (x:xs) = (foldl' f $! f a x) xs

>instance (Functor f) => InitialAlgebra f (Rec f) where
>	  unCreate = unIn

>instance (Functor f) => InitialAlgebra f (CoRec f) where
>	  unCreate = unStrictIn

>instance (Functor f) => FinalCoalgebra f (Rec f) where
>	  unDestroy = In

>while :: (a -> a) -> (a -> Bool) -> a -> [a]
>f `while` p = takeWhile p . iterate f

>whileM :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m [a]
>(f `whileM` p) x | p x = do { v <- f x ; w <- (f `whileM` p) v ; return (v:w) }
>                 | otherwise = return []

>instance Show (Rec Maybe) where
>  show x = "Natural[" ++ show (count_just x) ++ "]"

>count_just :: Rec Maybe -> Integer
>count_just = visit $ recfold (maybe 0 succ)
