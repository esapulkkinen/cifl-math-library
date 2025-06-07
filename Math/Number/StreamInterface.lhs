>{-# LANGUAGE TypeFamilies, RankNTypes, QuantifiedConstraints #-}
>{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds #-}
>{-# LANGUAGE FlexibleContexts, TypeOperators, FlexibleInstances #-}
>{-# LANGUAGE LinearTypes, BangPatterns #-}
>module Math.Number.StreamInterface where
>import Math.Matrix.Interface
>import Data.Kind
>import Control.Applicative
>
>infixr 5 `pre`

>-- | Idea: 'limit' produces an exact value from a monotone sequence of
>-- approximations
>--
>-- <https://ncatlab.org/nlab/show/closure+operator>
>--
>-- The Closure represents a sequentially closed set of elements
>class (Monad str, StreamBuilder str, StreamObserver str) => Limiting str a where
>  data Closure str a
>  limit          :: str a -> Closure str a
>  approximations :: Closure str a -> str a

>class (Fractional a,Limiting str a) => Infinitesimal str a where
>  epsilonClosure :: Closure str a
>  infiniteClosure :: Closure str a
>  minusInfiniteClosure :: Closure str a
>  nanClosure :: Closure str a
>  epsilonStream :: str a
>  infiniteStream :: str a
>  minusInfiniteStream :: str a
>  epsilonClosure = limit epsilonStream
>  epsilonStream = approximations epsilonClosure
>  infiniteClosure = limit $ fmap (1/) epsilonStream
>  infiniteStream = approximations infiniteClosure
>  minusInfiniteClosure = limit $ fmap (negate . (1/)) epsilonStream
>  minusInfiniteStream = approximations minusInfiniteClosure
>  nanClosure = epsilonClosure
>  {-# MINIMAL (epsilonClosure | epsilonStream) #-}

>class (Applicative str) => StreamBuilder str where
>  pre :: a -> str a -> str a

>class (StreamBuilder str) => StreamBuilder2 str where
>  pre2 :: str a -> (str :*: str) a -> (str :*: str) a -> (str :*: str) a

>class (Applicative str) => StreamObserver str where
>  shead :: str a -> a
>  stail :: str a -> str a

>class (StreamObserver str) => StreamObserver2 str where
>  stail2 :: str a -> str a

>class (StreamObserver str) => StreamObserver3 str where
>  stail3 :: str a -> str a

>infixr 5 `Pre`

>-- | Data structure of infinite lazy streams.
>data Stream a = Pre { sheadImpl :: a, stailLazy :: Stream a }

>class (Infinitesimal Stream a) => Closed a where
>  accumulationPoint :: Closure Stream a -> a

>type DerivateConstraint str a = (Monad str, Num a)

>-- | \[ {\rm{partial\_derivate}}(\Delta, f, x) = \lim_{\epsilon \rightarrow 0}{{f(\Delta_{\epsilon}(x)) - f(x)}\over{\Delta_{\epsilon}(x) - x}} \]
>partialDerivate :: (Closed eps, Fractional eps)
> => (eps -> a -> a) -> (a -> eps) -> (a -> eps)
>partialDerivate delta f = \v -> accumulationPoint $ limit $ do
>   let fv = f v
>   eps <- epsilonStream
>   return $! (f (delta eps v) - fv) / eps


>-- | \[ {\rm{partial\_derive}}(\Delta, f, x) = \lim_{\epsilon \rightarrow 0}{{f(\Delta_{\epsilon}(x)) - f(x)}\over{\Delta_{\epsilon}(x) - x}} \]
>partialDerive :: (Closed a, Infinitesimal Stream a, Fractional a) => (a -> a -> a) -> (a -> a) -> a -> a
>partialDerive delta f = \v -> accumulationPoint $ limit $ do
>   let fv = f v
>   eps <- epsilonStream
>   let vPlusDv = delta eps v
>   return $! (f vPlusDv - fv) / (vPlusDv - v)

>-- | derivate_around doesn't require 'f' to be defined at 'x', but requires
>-- limits from both sides of 'x' to exist [it never evaluates 'f' at
>-- 'x'].
>-- \[ \lim_{\epsilon \rightarrow 0} {{f(x+\epsilon)-f(x-\epsilon)}\over{2\epsilon}} \]

>derivateAround :: (Infinitesimal str a) => (a -> a) -> a -> Closure str a
>derivateAround f = \x -> limit $ do
>    eps <- epsilonStream
>    return $! (f (x + eps) - f (x - eps)) / (2 * eps)


>vectorDerivate :: (Infinitesimal str (Scalar a), VectorSpace a, Limiting str a)
> => (Scalar a -> a) -> Scalar a -> Closure str a
>vectorDerivate f = \x -> limit $ do
>   let fx = f x -- optimization
>   eps <- epsilonStream
>   return $! (1 / eps) %* (f (x + eps) %- fx)

>derivateClosed :: (Infinitesimal str a) => (a -> a) -> a -> Closure str a
>derivateClosed f = \x -> let fx = f x in limit $ do
>    eps <- epsilonStream
>    return $! (f (x + eps) - fx) / eps

>newtonsMethodReal :: (Infinitesimal Stream a, Closed a) => (a -> a) -> a -> Closure Stream a
>newtonsMethodReal f = \x -> limit $ iterateStream iteration x
>   where iteration z' = z' - f z' / accumulationPoint (derivateClosed f z')

>{-# INLINEABLE iterateStream #-}
>iterateStream :: (StreamBuilder str) => (a -> a) -> a -> str a
>iterateStream f = \x -> pre x $ iterateStream f (f x)


>-- | <https://en.wikipedia.org/wiki/Differential_calculus>
>pseudoDerivate :: (Fractional r, Limiting str r, Infinitesimal str a)
>                => (a -> r) -> a -> Closure str r
>pseudoDerivate f = \x -> limit $ do
>    let fx = f x
>    eps <- epsilonStream
>    return $! (f (x + eps) - fx) / f eps

>-- | \[\lim_{\epsilon\rightarrow 0}{{f(a+\epsilon,b)-f(a,b)}\over{\epsilon}}\]
>partialDerivate1_2 :: (Infinitesimal str a)
>                    => (a -> b -> a) -> a -> b -> Closure str a
>partialDerivate1_2 f a b = limit $ do
>    let fab = f a b
>    eps <- epsilonStream
>    return $! (f (a + eps) b - fab) / eps

>-- | \[\lim_{\epsilon\rightarrow 0}{{f(a,b+\epsilon)-f(a,b)}\over{\epsilon}}\]
>partialDerivate2_2 :: (Infinitesimal str a) => (b -> a -> a) -> b -> a -> Closure str a
>partialDerivate2_2 f a b = limit $ do
>    let fab = f a b
>    eps <- epsilonStream
>    return $! (f a (b + eps) - fab) / eps

>-- | \[\lim_{\epsilon\rightarrow 0}{{f(a+\epsilon,b,c)-f(a,b,c)}\over{\epsilon}}\]
>partialDerivate1_3 :: (Infinitesimal str a)
>                    => (a -> b -> c -> a) -> a -> b -> c -> Closure str a
>partialDerivate1_3 f a b c = limit $ do
>    let fabc = f a b c
>    eps <- epsilonStream
>    return $! (f (a + eps) b c - fabc) / eps

>-- | \[\lim_{\epsilon\rightarrow 0}{{f(a,b+\epsilon,c)-f(a,b,c)}\over{\epsilon}}\]
>partialDerivate2_3 :: (Infinitesimal str a)
>                    => (b -> a -> c -> a) -> b -> a -> c -> Closure str a
>partialDerivate2_3 f a b c = limit $ do
>    let fabc = f a b c
>    eps <- epsilonStream
>    return $! (f a (b + eps) c - fabc) / eps

>-- | \[\lim_{\epsilon\rightarrow 0}{{f(a,b,c+\epsilon)-f(a,b,c)}\over{\epsilon}}\]
>partialDerivate3_3 :: (Infinitesimal str a)
>                    => (b -> c -> a -> a) -> b -> c -> a -> Closure str a
>partialDerivate3_3 f a b c = limit $ do
>    let fabc = f a b c
>    eps <- epsilonStream
>    return $! (f a b (c + eps) - fabc) / eps

>instance (Num a) => VectorSpace (Stream a) where
>   type Scalar (Stream a) = a
>   vzero = pure 0
>   vnegate = liftA negate
>   x %+ y = liftA2 (+) x y
>   x %* s = liftA (x *) s

>instance Applicative Stream where
>   pure = constant
>   (Pre f fr) <*> (Pre b br) = Pre (f b) (fr <*> br)

>-- | stream consisting of the same element repeated.
>{-# INLINE constant #-}
>constant :: a -> Stream a
>constant x = Pre x (constant x)

>instance Functor Stream where
>   fmap f (Pre x xr) = Pre (f x) (fmap f xr)

>streamMatrixImpl :: Stream a -> Stream (Stream a, Stream a) -> (Stream :*: Stream) a
>streamMatrixImpl (Pre x xr) (Pre (y,yh) yr) = Matrix $ Pre (Pre x y)
>                             (liftA2 Pre yh (cells (streamMatrixImpl xr yr)))

>-- Notice this older version doesn't need Closed or Num constraints
>streamDiagonalImpl :: (Stream :*: Stream) a -> Stream a
>streamDiagonalImpl (Matrix (Pre (Pre x _) dr)) 
>   = Pre x $ sdiag $ fmap stail dr
>  where sdiag (Pre (Pre x' _) dr') = Pre x' $ sdiag (fmap stail dr')

>streamCodiagonalImpl :: (Stream :*: Stream) a -> Stream (Stream a, Stream a)
>streamCodiagonalImpl (Matrix (Pre (Pre _ x) yr))
> = Pre (x,fmap shead yr) (streamCodiagonalImpl $ Matrix $ fmap stail yr)

>instance CodiagonalMatrix Stream a where
>   data Codiagonal Stream a = CodiagonalStream { codiagonalSubstreams :: Stream (Stream a, Stream a) }
>   type (Stream \\ a) = Stream a
>   codiagonalImpl = CodiagonalStream . streamCodiagonalImpl
>   diag |\| (CodiagonalStream codiag) = streamMatrixImpl diag codiag
>   downProject (CodiagonalStream ~(Pre ~(x,_) _)) = x
>   rightProject (CodiagonalStream ~(Pre ~(_,y) _)) = y

>instance Transposable Stream Stream a where
>  transposeImpl x = streamMatrixImpl (streamDiagonalImpl x)
>                     (fmap swap $ codiagonalSubstreams $ codiagonalImpl x)
>    where swap (a,b) = (b,a)

>instance StreamObserver Stream where
>   shead = sheadImpl
>   stail = stailLazy

