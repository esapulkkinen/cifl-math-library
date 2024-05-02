>{-# LANGUAGE GADTs, TypeFamilies #-}
>module Math.Number.SymbolicStream where
>import Math.Matrix.Interface
>import Math.Number.Interface
>import Control.Applicative
>import safe qualified Data.Sequence as Seq
> 
>data SymbolicStream a where
>  ConstantStream :: a -> SymbolicStream a
>  PreStream :: a -> SymbolicStream a -> SymbolicStream a

>instance (Show a) => Show (SymbolicStream a) where
>  show (ConstantStream x) = show x ++ "*"
>  show (PreStream x xr) = show x ++ "," ++ show xr


>instance Functor SymbolicStream where
>  fmap f (ConstantStream x) = ConstantStream (f x)
>  fmap f (PreStream x xr) = PreStream (f x) (fmap f xr)

>instance (Num a) => VectorSpace (SymbolicStream a) where
>   type Scalar (SymbolicStream a) = a
>   vzero = ConstantStream 0
>   vnegate (ConstantStream x) = ConstantStream (negate x)
>   vnegate (PreStream x xr) = PreStream (negate x) (vnegate xr)
>   (ConstantStream x) %+ (ConstantStream y) = ConstantStream (x + y)
>   z@(ConstantStream x) %+ (PreStream x' xr) = PreStream (x + x') $ z %+ xr
>   (PreStream x xr) %+ z@(ConstantStream y) = PreStream (x + y) $ xr %+ z
>   (PreStream x xr) %+ (PreStream y yr) = PreStream (x + y) $ xr %+ yr
>   k %* (ConstantStream x) = ConstantStream (k * x)
>   k %* (PreStream x xr) = PreStream (k * x) (k %* xr)

>sy_z :: (Num a) => SymbolicStream a
>sy_z = PreStream 0 (PreStream 1 vzero)

>sy_z2 :: (Num a) => SymbolicStream (SymbolicStream a)
>sy_z2 = PreStream sy_z vzero

>instance Applicative SymbolicStream where
>  pure x = ConstantStream x
>  (ConstantStream f) <*> (ConstantStream x) = ConstantStream (f x)
>  (PreStream f fr) <*> xx@(ConstantStream x) = PreStream (f x) $ fr <*> xx
>  ff@(ConstantStream f) <*> (PreStream x xr) = PreStream (f x) $ ff <*> xr
>  (PreStream f fr) <*> (PreStream x xr) = PreStream (f x) $  fr <*> xr

>instance (Num a) => Num (SymbolicStream a) where
>  (+) = liftA2 (+)
>  (-) = liftA2 (-)
>  negate = liftA negate
>  abs = liftA abs
>  signum = liftA signum

>codiagonals_seq :: (SymbolicStream :*: SymbolicStream) a -> SymbolicStream (Seq.Seq a)
>codiagonals_seq q = PreStream (Seq.singleton d) $ Pre (Seq.singleton x Seq.|> y) $ liftA3 f xr yr $ codiagonals_seq m
>  where f pr suf r = (pr Seq.<| r) Seq.|> suf

>stail :: SymbolicStream a -> SymbolicStream a
>stail z@(ConstantStream x) = z
>stail (PreStream x xr) = xr

>shead :: SymbolicStream a -> a
>shead (ConstantStream x) = x
>shead (PreStream x _) = x

>stream_diagonal_impl :: (SymbolicStream :*: SymbolicStream) a -> SymbolicStream a
>stream_diagonal_impl (Matrix (PreStream (PreStream x _) dr))
>   = PreStream x $ stream_diagonal_impl $ Matrix $ fmap stail dr
>stream_diagonal_impl (Matrix (ConstantStream (ConstantStream x)))
>   = ConstantStream x
>stream_diagonal_impl (Matrix (ConstantStream (PreStream x xr)))
>   = PreStream x $ stream_diagonal_impl $ Matrix (ConstantStream xr)
>stream_diagonal_impl (Matrix (PreStream (ConstantStream x) dr))
>   = PreStream x $ stream_diagonal_impl $ fmap stail dr
>
>stream_codiagonal_impl :: (SymbolicStream :*: SymbolicStream) a
> -> SymbolicStream (SymbolicStream a, SymbolicStream a)
>stream_codiagonal_impl (Matrix (PreStream (PreStream x _) dr))
>   = PreStream (x, fmap shead dr) (stream_codiagonal_impl $ Matrix $ fmap stail dr)
>stream_codiagonal_impl (Matrix (ConstantStream (PreStream x xr)))
>   = 

>stream_codiagonal_impl
> 
>dematrix_impl :: (SymbolicStream :*: SymbolicStream) a
>  -> (a,(SymbolicStream a, SymbolicStream a),
>     (SymbolicStream :*: SymbolicStream) a)
>dematrix_impl y 
