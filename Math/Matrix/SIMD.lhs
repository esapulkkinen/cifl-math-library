>{-# LANGUAGE CPP #-}
>{-# LANGUAGE GADTs, FlexibleInstances, TypeFamilies, DeriveFunctor #-}
>{-# LANGUAGE MagicHash #-}
>-- | This modules provides SIMD optimized versions of vectors.
>-- Relies heavily on GHC SIMD support. SIMD works only when using LLVM and
>-- GHC 8.x
>module Math.Matrix.SIMD where
>
>import Control.Applicative
>import Math.Matrix.Interface

#if __GLASGOW_HASKELL__ >= 800 && __GLASGOW_HASKELL_LLVM__ >= 500

>{-# OPTIONS_GHC -fllvm #-}

>import GHC.Exts
>import GHC.Int
>import GHC.Word

>data SVec4 a where
>   Int32SVec4 :: Int32X4# -> SVec4 Int32

>data SVec8 a where
>   Int16SVec8 :: Int16X8# -> SVec8 Int16
>   Int32SVec8 :: Int32X8# -> SVec8 Int32

>data SVec16 a where
>   Int8SVec16  :: Int8X16# -> SVec16 Int8
>   Int16SVec16 :: Int16X16# -> SVec16 Int16

#if __GLASGOW_HASKELL_LLVM__ >= 500

>   Int32SVec16 :: Int32X16# -> SVec16 Int32

#endif

>instance VectorSpace (SVec4 Int32) where
>   type Scalar (SVec4 Int32) = Int32
>   vzero = Int32SVec4 (broadcastInt32X4# 0#)
>   vnegate (Int32SVec4 i) = Int32SVec4 (negateInt32X4# i)
>   (Int32SVec4 x) %+ (Int32SVec4 y) = Int32SVec4 (plusInt32X4# x y)
>   (I32# x) %* (Int32SVec4 v) = Int32SVec4 (timesInt32X4# (broadcastInt32X4# x) v)

>instance VectorSpace (SVec8 Int16) where
>   type Scalar (SVec8 Int16) = Int16
>   vzero = Int16SVec8 (broadcastInt16X8# 0#)
>   vnegate (Int16SVec8 i) = Int16SVec8 (negateInt16X8# i)
>   (Int16SVec8 x) %+ (Int16SVec8 y) = Int16SVec8 (plusInt16X8# x y)
>   (I16# x) %* (Int16SVec8 v) = Int16SVec8 (timesInt16X8# (broadcastInt16X8# x) v)

>instance VectorSpace (SVec8 Int32) where
>   type Scalar (SVec8 Int32) = Int32
>   vzero = Int32SVec8 (broadcastInt32X8# 0#)
>   vnegate (Int32SVec8 i) = Int32SVec8 (negateInt32X8# i)
>   (Int32SVec8 x) %+ (Int32SVec8 y) = Int32SVec8 (plusInt32X8# x y)
>   (I32# x) %* (Int32SVec8 v) = Int32SVec8 (timesInt32X8# (broadcastInt32X8# x) v)

>instance VectorSpace (SVec16 Int8) where
>   type Scalar (SVec16 Int8) = Int8
>   vzero = Int8SVec16 (broadcastInt8X16# 0#)
>   vnegate (Int8SVec16 i) = Int8SVec16 (negateInt8X16# i)
>   (Int8SVec16 x) %+ (Int8SVec16 y) = Int8SVec16 (plusInt8X16# x y)
>   (I8# x) %* (Int8SVec16 v) = Int8SVec16 (timesInt8X16# (broadcastInt8X16# x) v)

>instance VectorSpace (SVec16 Int16) where
>   type Scalar (SVec16 Int16) = Int16
>   vzero = Int16SVec16 (broadcastInt16X16# 0#)
>   vnegate (Int16SVec16 i) = Int16SVec16 (negateInt16X16# i)
>   (Int16SVec16 x) %+ (Int16SVec16 y) = Int16SVec16 (plusInt16X16# x y)
>   (I16# x) %* (Int16SVec16 v) = Int16SVec16 (timesInt16X16# (broadcastInt16X16# x) v)

#if __GLASGOW_HASKELL_LLVM__ >= 500

>instance VectorSpace (SVec16 Int32) where
>   type Scalar (SVec16 Int32) = Int32
>   vzero = Int32SVec16 (broadcastInt32X16# 0#)
>   vnegate (Int32SVec16 i) = Int32SVec16 (negateInt32X16# i)
>   (Int32SVec16 x) %+ (Int32SVec16 y) = Int32SVec16 (plusInt32X16# x y)
>   (I32# x) %* (Int32SVec16 v) = Int32SVec16 (timesInt32X16# (broadcastInt32X16# x) v)

#endif

#else

>data SVec8 a = SVec8 a a a a a a a a
> deriving (Functor)
>data SVec4 a = SVec4 a a a a
> deriving (Functor)
>data SVec16 a = SVec16 a a a a a a a a a a a a a a a a
> deriving (Functor)

>instance Applicative SVec4 where
>   pure x = SVec4 x x x x 
>   (SVec4 f1 f2 f3 f4) <*> (SVec4 x1 x2 x3 x4)
>     = SVec4 (f1 x1) (f2 x2) (f3 x3) (f4 x4)

>instance Applicative SVec8 where
>   pure x = SVec8 x x x x x x x x
>   (SVec8 f1 f2 f3 f4 f5 f6 f7 f8) <*> (SVec8 x1 x2 x3 x4 x5 x6 x7 x8)
>     = SVec8 (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5) (f6 x6) (f7 x7) (f8 x8)

>instance Applicative SVec16 where
>   pure x = SVec16 x x x x x x x x x x x x x x x x
>   (SVec16 f1 f2 f3 f4 f5 f6 f7 f8 f1' f2' f3' f4' f5' f6' f7' f8')
>     <*> (SVec16 x1 x2 x3 x4 x5 x6 x7 x8 x1' x2' x3' x4' x5' x6' x7' x8')
>      = SVec16 (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5) (f6 x6) (f7 x7) (f8 x8)
>             (f1' x1') (f2' x2') (f3' x3') (f4' x4') (f5' x5') (f6' x6') (f7' x7') (f8' x8')

>instance (Num a) => VectorSpace (SVec4 a) where
>   type Scalar (SVec4 a) = a
>   vzero = SVec4 0 0 0 0 
>   vnegate = fmap negate
>   x %+ y = liftA2 (+) x y
>   x %* y = fmap (x *) y 
>instance (Num a) => VectorSpace (SVec8 a) where
>   type Scalar (SVec8 a) = a
>   vzero = SVec8 0 0 0 0 0 0 0 0
>   vnegate = fmap negate
>   x %+ y = liftA2 (+) x y
>   x %* y = fmap (x *) y 
>instance (Num a) => VectorSpace (SVec16 a) where
>   type Scalar (SVec16 a) = a
>   vzero = SVec16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
>   vnegate = fmap negate
>   x %+ y = liftA2 (+) x y
>   x %* y = fmap (x *) y 

#endif
