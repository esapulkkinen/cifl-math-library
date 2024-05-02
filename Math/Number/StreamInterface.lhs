>{-# LANGUAGE TypeFamilies, RankNTypes, QuantifiedConstraints #-}
>{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds #-}
>{-# LANGUAGE FlexibleContexts, TypeOperators, FlexibleInstances #-}
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
>  epsilon_closure :: Closure str a
>  infinite_closure :: Closure str a
>  minus_infinite_closure :: Closure str a
>  nan_closure :: Closure str a
>  epsilon_stream :: str a
>  infinite_stream :: str a
>  minus_infinite_stream :: str a
>  epsilon_closure = limit epsilon_stream
>  epsilon_stream = approximations epsilon_closure
>  infinite_closure = limit $ fmap (1/) epsilon_stream
>  infinite_stream = approximations infinite_closure
>  minus_infinite_closure = limit $ fmap (negate . (1/)) epsilon_stream
>  minus_infinite_stream = approximations minus_infinite_closure
>  nan_closure = epsilon_closure
>  {-# MINIMAL (epsilon_closure | epsilon_stream) #-}

>class (Applicative str) => StreamBuilder str where
>  pre :: a -> str a -> str a

>class (Applicative str) => StreamObserver str where
>  shead :: str a -> a
>  stail :: str a -> str a

>infixr 5 `Pre`

>-- | Data structure of infinite lazy streams.
>data Stream a = Pre { shead_impl :: a, stail_lazy :: Stream a }

>class (Infinitesimal Stream a) => Closed a where
>  accumulation_point :: Closure Stream a -> a

>type DerivateConstraint str a = (Monad str, Num a)

>-- | \[ {\rm{partial\_derivate}}(\Delta, f, x) = \lim_{\epsilon \rightarrow 0}{{f(\Delta_{\epsilon}(x)) - f(x)}\over{\Delta_{\epsilon}(x) - x}} \]
>partial_derivate :: (Closed eps, Fractional eps)
> => (eps -> a -> a) -> (a -> eps) -> (a -> eps)
>partial_derivate delta f = \v -> accumulation_point $ limit $ do
>   let fv = f v
>   eps <- epsilon_stream
>   return $! (f (delta eps v) - fv) / eps


>-- | \[ {\rm{partial\_derive}}(\Delta, f, x) = \lim_{\epsilon \rightarrow 0}{{f(\Delta_{\epsilon}(x)) - f(x)}\over{\Delta_{\epsilon}(x) - x}} \]
>partial_derive :: (Closed a, Infinitesimal Stream a, Fractional a) => (a -> a -> a) -> (a -> a) -> a -> a
>partial_derive delta f = \v -> accumulation_point $ limit $ do
>   let fv = f v
>   eps <- epsilon_stream
>   let v_plus_dv = delta eps v
>   return $! (f v_plus_dv - fv) / (v_plus_dv - v)

>-- | derivate_around doesn't require 'f' to be defined at 'x', but requires
>-- limits from both sides of 'x' to exist [it never evaluates 'f' at
>-- 'x'].
>-- \[ \lim_{\epsilon \rightarrow 0} {{f(x+\epsilon)-f(x-\epsilon)}\over{2\epsilon}} \]

>derivate_around :: (Infinitesimal str a) => (a -> a) -> a -> Closure str a
>derivate_around f = \x -> limit $ do
>    eps <- epsilon_stream
>    return $! (f (x + eps) - f (x - eps)) / (2 * eps)


>vector_derivate :: (Infinitesimal str (Scalar a), VectorSpace a, Limiting str a)
> => (Scalar a -> a) -> Scalar a -> Closure str a
>vector_derivate f = \x -> limit $ do
>   let fx = f x -- optimization
>   eps <- epsilon_stream
>   return $! (1 / eps) %* (f (x + eps) %- fx)

>derivate_closed :: (Infinitesimal str a) => (a -> a) -> a -> Closure str a
>derivate_closed f = \x -> let fx = f x in limit $ do
>    eps <- epsilon_stream
>    return $! (f (x + eps) - fx) / eps

>newtons_method_real :: (Infinitesimal Stream a, Closed a) => (a -> a) -> a -> Closure Stream a
>newtons_method_real f = \x -> limit $ iterate_stream iteration x
>   where iteration z' = z' - f z' / accumulation_point (derivate_closed f z')

>{-# INLINEABLE iterate_stream #-}
>iterate_stream :: (StreamBuilder str) => (a -> a) -> a -> str a
>iterate_stream f = \x -> pre x $ iterate_stream f (f x)


>-- | <https://en.wikipedia.org/wiki/Differential_calculus>
>pseudo_derivate :: (Fractional r, Limiting str r, Infinitesimal str a)
>                => (a -> r) -> a -> Closure str r
>pseudo_derivate f = \x -> limit $ do
>    let fx = f x
>    eps <- epsilon_stream
>    return $! (f (x + eps) - fx) / f eps

>-- | \[\lim_{\epsilon\rightarrow 0}{{f(a+\epsilon,b)-f(a,b)}\over{\epsilon}}\]
>partial_derivate1_2 :: (Infinitesimal str a)
>                    => (a -> b -> a) -> a -> b -> Closure str a
>partial_derivate1_2 f a b = limit $ do
>    let fab = f a b
>    eps <- epsilon_stream
>    return $! (f (a + eps) b - fab) / eps

>-- | \[\lim_{\epsilon\rightarrow 0}{{f(a,b+\epsilon)-f(a,b)}\over{\epsilon}}\]
>partial_derivate2_2 :: (Infinitesimal str a) => (b -> a -> a) -> b -> a -> Closure str a
>partial_derivate2_2 f a b = limit $ do
>    let fab = f a b
>    eps <- epsilon_stream
>    return $! (f a (b + eps) - fab) / eps

>-- | \[\lim_{\epsilon\rightarrow 0}{{f(a+\epsilon,b,c)-f(a,b,c)}\over{\epsilon}}\]
>partial_derivate1_3 :: (Infinitesimal str a)
>                    => (a -> b -> c -> a) -> a -> b -> c -> Closure str a
>partial_derivate1_3 f a b c = limit $ do
>    let fabc = f a b c
>    eps <- epsilon_stream
>    return $! (f (a + eps) b c - fabc) / eps

>-- | \[\lim_{\epsilon\rightarrow 0}{{f(a,b+\epsilon,c)-f(a,b,c)}\over{\epsilon}}\]
>partial_derivate2_3 :: (Infinitesimal str a)
>                    => (b -> a -> c -> a) -> b -> a -> c -> Closure str a
>partial_derivate2_3 f a b c = limit $ do
>    let fabc = f a b c
>    eps <- epsilon_stream
>    return $! (f a (b + eps) c - fabc) / eps

>-- | \[\lim_{\epsilon\rightarrow 0}{{f(a,b,c+\epsilon)-f(a,b,c)}\over{\epsilon}}\]
>partial_derivate3_3 :: (Infinitesimal str a)
>                    => (b -> c -> a -> a) -> b -> c -> a -> Closure str a
>partial_derivate3_3 f a b c = limit $ do
>    let fabc = f a b c
>    eps <- epsilon_stream
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

>stream_matrix_impl :: Stream a -> Stream (Stream a, Stream a) -> (Stream :*: Stream) a
>stream_matrix_impl ~(Pre x xr) ~(Pre ~(y,yh) yr) = Matrix $ Pre (Pre x y)
>                             (liftA2 Pre yh (cells (stream_matrix_impl xr yr)))

>-- Notice this older version doesn't need Closed or Num constraints
>stream_diagonal_impl :: (Stream :*: Stream) a -> Stream a
>stream_diagonal_impl (Matrix (Pre (Pre x _) dr)) 
>   = Pre x $ stream_diagonal_impl $ Matrix $ fmap stail dr

>stream_codiagonal_impl :: (Stream :*: Stream) a -> Stream (Stream a, Stream a)
>stream_codiagonal_impl (Matrix (Pre (Pre _ x) yr))
> = Pre (x,fmap shead yr) (stream_codiagonal_impl $ Matrix $ fmap stail yr)

>instance (Num a) => CodiagonalMatrix Stream a where
>   data Codiagonal Stream a = CodiagonalStream { codiagonal_substreams :: Stream (Stream a, Stream a) }
>   type (Stream \\ a) = Stream a
>   codiagonal_impl = CodiagonalStream . stream_codiagonal_impl
>   diag |\| (CodiagonalStream codiag) = stream_matrix_impl diag codiag
>   down_project (CodiagonalStream ~(Pre ~(x,_) _)) = x
>   right_project (CodiagonalStream ~(Pre ~(_,y) _)) = y

>instance (Num a) => Transposable Stream Stream a where
>  transpose_impl x = stream_matrix_impl (stream_diagonal_impl x)
>                     (fmap swap $ codiagonal_substreams $ codiagonal_impl x)
>    where swap (a,b) = (b,a)

>instance StreamObserver Stream where
>   shead = shead_impl
>   stail = stail_lazy

