>{-# LANGUAGE CPP #-}
>{-# LANGUAGE GADTs, FlexibleInstances, TypeFamilies, DeriveFunctor, FlexibleContexts #-}
>{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
>{-# LANGUAGE MagicHash, UnboxedTuples #-}
>{-# LANGUAGE Trustworthy #-}
>{-# LANGUAGE TypeFamilies #-}
>{-# LANGUAGE AllowAmbiguousTypes #-}
>{-# LANGUAGE MultiParamTypeClasses #-}
>{-# LANGUAGE RoleAnnotations #-}
>{-# LANGUAGE DataKinds, IncoherentInstances #-}
>{-# OPTIONS_HADDOCK ignore-exports #-}
>-- | This modules provides SIMD optimized versions of vectors.
>-- Relies heavily on GHC SIMD support. SIMD works only when using LLVM and
>-- GHC 8.x
>-- The only combinations of vectors supported are
>-- 4 * Int32, 8 * Int16 and 16 * Int8, 4*Float, 2*Double
>-- Conversions to and from optimized types and
>-- Vector4 Int32, (Vector2 :*: Vector2) Int32, (Vector4 Int16, Vector4 Int16)
>-- Vector4 Float, (Vector2 :*: Vector2) Float,
>-- and (Vector4 :*: Vector4) Int8 can be performed as appropriate for each type.
>module Math.Matrix.SIMD
>  (SIMDVec, -- hide constructors
>   Optimal(..),Optimized(..),
>   makeSVec4,makeSVec8,makeSVec16, makeFVec4,
>   fromInt32Vector4,toInt32Vector4, 
>   fromInt16Vector8, toInt16Vector8,
>   fromInt8Vector16, toInt8Vector16,
>   fromDoubleVector2, toDoubleVector2,
>   fromFloatVector4, toFloatVector4,
>   from2x2Matrix, to2x2Matrix,
>   from2x2FloatVector4,to2x2FloatVector4,
>   transposeFloatVector4,
>   eqSVec4,eqSVec8,eqSVec16,
>   zipSVec4,zipSVec8,zipSVec16,zipFVec4,
>   mapSVec4,mapSVec8,mapSVec16,mapFVec4,
>   constantSVec4,constantSVec8,constantSVec16,constantFVec4,
>   fvec4_x,fvec4_y,fvec4_z,fvec4_t,
>   svec4_x,svec4_y,svec4_z,svec4_t,
>   svec8_1,svec8_2,svec8_3,svec8_4,svec8_5,svec8_6,svec8_7,svec8_8,
>   svec16_1,svec16_2,svec16_3,svec16_4,svec16_5,svec16_6,svec16_7,svec16_8,
>   svec16_9,svec16_10,svec16_11,svec16_12,svec16_13,svec16_14,svec16_15,svec16_16,
>   fast_multiply4_float, fast_multiply2_double
>  )
> where
>
>import Data.Complex
>import Control.Applicative
>import Math.Matrix.Interface
>import Math.Matrix.Vector4
>import Math.Matrix.Vector3
>import Math.Matrix.Vector2

#if __GLASGOW_HASKELL__ >= 800 && defined(WITH_LLVM)

{-# OPTIONS_GHC -fllvm #-}

>import GHC.Exts
>import GHC.Int
>import GHC.Word
>import GHC.TypeLits

>data SIMDVec (len :: Nat) a where
>   Int64SVec2 :: !Int64X2# -> SIMDVec 2 Int64
>   Int32SVec4 :: !Int32X4# -> SIMDVec 4 Int32
>   Int16SVec8 :: !Int16X8# -> SIMDVec 8 Int16
>   Int8SVec16 :: !Int8X16# -> SIMDVec 16 Int8
>   FloatFVec4 :: !FloatX4# -> SIMDVec 4 Float
>   DoubleFVec2 :: !DoubleX2# -> SIMDVec 2 Double
>   ComplexVec2 :: !FloatX4# -> SIMDVec 2 (Complex Float)
>   ComplexVec1 :: !DoubleX2# -> SIMDVec 1 (Complex Double)

>conjSIMD2 :: SIMDVec 2 (Complex Float) -> SIMDVec 2 (Complex Float)
>conjSIMD2 (ComplexVec2 x) = ComplexVec2 x'
>  where (# a,b,c,d #) = unpackFloatX4# x
>        bneg = negateFloat# b
>        dneg = negateFloat# d
>        x' = packFloatX4# (# a,bneg,c,dneg #)

>conjSIMD1 :: SIMDVec 1 (Complex Double) -> SIMDVec 1 (Complex Double)
>conjSIMD1 (ComplexVec1 x) = ComplexVec1 x'
>   where (# a,b #) = unpackDoubleX2# x
>         bneg = negateDouble# b
>         x' = packDoubleX2# (# a,bneg #)

>{-# INLINABLE sum_coordinates2Complex #-}
>sum_coordinates2Complex :: SIMDVec 2 (Complex Float) -> Complex Float
>sum_coordinates2Complex (ComplexVec2 x) = F# (plusFloat# a c) :+ F# (plusFloat# b d)
>   where (# a,b,c,d #) = unpackFloatX4# x

>{-# INLINABLE sum_coordinates1Complex #-}
>sum_coordinates1Complex :: SIMDVec 1 (Complex Double) -> Complex Double
>sum_coordinates1Complex (ComplexVec1 x) = D# a :+ D# b
>   where (# a,b #) = unpackDoubleX2# x

>{-# INLINABLE sum_coordinates1Complex_components #-}
>sum_coordinates1Complex_components :: SIMDVec 1 (Complex Double) -> Double
>sum_coordinates1Complex_components (ComplexVec1 x) = D# (a +## b)
>   where (# a,b #) = unpackDoubleX2# x

>{-# INLINABLE sum_coordinates2Int #-}
>sum_coordinates2Int :: SIMDVec 2 Int64 -> Int64
>sum_coordinates2Int (Int64SVec2 x) = I64# (a `plusInt64#` b)
>   where (# a,b #) = unpackInt64X2# x

>{-# INLINABLE sum_coordinates4Int #-}
>sum_coordinates4Int :: SIMDVec 4 Int32 -> Int32
>sum_coordinates4Int (Int32SVec4 x) = I32# ((a `plusInt32#` b) `plusInt32#` (c `plusInt32#` d))
>   where (# a,b,c,d #) = unpackInt32X4# x

>{-# INLINABLE sum_coordinates8Int #-}
>sum_coordinates8Int :: SIMDVec 8 Int16 -> Int16
>sum_coordinates8Int (Int16SVec8 x) = I16# (abcd `plusInt16#` efgh)
>   where (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x
>         ab = a `plusInt16#` b
>         cd = c `plusInt16#` d
>         ef = e `plusInt16#` f
>         gh = g `plusInt16#` h
>         abcd = ab `plusInt16#` cd
>         efgh = ef `plusInt16#` gh
> 
>{-# INLINABLE sum_coordinates16Int #-}
>sum_coordinates16Int :: SIMDVec 16 Int8 -> Int8
>sum_coordinates16Int (Int8SVec16 x) = I8# (abcdefgh `plusInt8#` abcdefgh2)
>   where (# a,b,c,d,e,f,g,h,a2,b2,c2,d2,e2,f2,g2,h2 #) = unpackInt8X16# x
>         ab = a `plusInt8#` b
>         cd = c `plusInt8#` d
>         ef = e `plusInt8#` f
>         gh = g `plusInt8#` h
>         abcd = ab `plusInt8#` cd
>         efgh = ef `plusInt8#` gh
>         abcdefgh = abcd `plusInt8#` efgh
>         ab2 = a2 `plusInt8#` b2
>         cd2 = c2 `plusInt8#` d2
>         ef2 = e2 `plusInt8#` f2
>         gh2 = g2 `plusInt8#` h2
>         abcd2 = ab2 `plusInt8#` cd2
>         efgh2 = ef2 `plusInt8#` gh2
>         abcdefgh2 = abcd2 `plusInt8#` efgh2

>{-# INLINABLE sum_coordinatesSIMD2 #-}
>sum_coordinatesSIMD2 :: SIMDVec 2 Double -> Double
>sum_coordinatesSIMD2 (DoubleFVec2 x) = D# (a +## b)
>  where (# a,b #) = unpackDoubleX2# x

>{-# INLINABLE sum_coordinatesSIMD4 #-}
>sum_coordinatesSIMD4 :: SIMDVec 4 Float -> Float
>sum_coordinatesSIMD4 (FloatFVec4 x) = F# (plusFloat# (plusFloat# a b) (plusFloat# c d))
>  where (# a,b,c,d #) = unpackFloatX4# x

>{-# INLINABLE sum_coordinatesSIMD3 #-}
>sum_coordinatesSIMD3 :: SIMDVec 4 Float -> Float
>sum_coordinatesSIMD3 (FloatFVec4 x) = F# (plusFloat# (plusFloat# a b) c)
>  where (# a,b,c,d #) = unpackFloatX4# x

>fromIntegralSIMD4 :: SIMDVec 4 Int32 -> SIMDVec 4 Float
>fromIntegralSIMD4 (Int32SVec4 x) = FloatFVec4 (packFloatX4# (# a',b',c',d' #))
>   where a' = int2Float# (int32ToInt# a)
>         b' = int2Float# (int32ToInt# b)
>         c' = int2Float# (int32ToInt# c)
>         d' = int2Float# (int32ToInt# d)
>         (# a,b,c,d #) = unpackInt32X4# x

>fromIntegralSIMD2 :: SIMDVec 2 Int64 -> SIMDVec 2 Double
>fromIntegralSIMD2 (Int64SVec2 x) = DoubleFVec2 (packDoubleX2# (# a',b' #))
>   where a' = int2Double# (int64ToInt# a)
>         b' = int2Double# (int64ToInt# b)
>         (# a,b #) = unpackInt64X2# x

>instance Num (SIMDVec 4 Float) where
>  (FloatFVec4 x) + (FloatFVec4 y) = FloatFVec4 (plusFloatX4# x y)
>  (FloatFVec4 x) - (FloatFVec4 y) = FloatFVec4 (minusFloatX4# x y)
>  (FloatFVec4 x) * (FloatFVec4 y) = FloatFVec4 (timesFloatX4# x y)
>  negate (FloatFVec4 x) = FloatFVec4 (negateFloatX4# x)
>  abs  = mapFVec4 abs
>  signum = mapFVec4 signum
>  fromInteger i = FloatFVec4 (broadcastFloatX4# f)
>    where F# f = fromInteger i

>instance Fractional (SIMDVec 4 Float) where
>   (FloatFVec4 x) / (FloatFVec4 y) = FloatFVec4 (divideFloatX4# x y)
>   recip (FloatFVec4 x) = FloatFVec4 (divideFloatX4# (broadcastFloatX4# 1.0#) x)
>   fromRational x = FloatFVec4 (broadcastFloatX4# f)
>     where (F# f) = fromRational x

>instance Num (SIMDVec 2 Double) where
>  (DoubleFVec2 x) + (DoubleFVec2 y) = DoubleFVec2 (plusDoubleX2# x y)
>  (DoubleFVec2 x) - (DoubleFVec2 y) = DoubleFVec2 (minusDoubleX2# x y)
>  (DoubleFVec2 x) * (DoubleFVec2 y) = DoubleFVec2 (timesDoubleX2# x y)
>  negate (DoubleFVec2 x) = DoubleFVec2 (negateDoubleX2# x)
>  abs = mapFVec2 abs
>  signum = mapFVec2 signum
>  fromInteger i = DoubleFVec2 (broadcastDoubleX2# f)
>    where D# f = fromInteger i

>instance Fractional (SIMDVec 2 Double) where
>   (DoubleFVec2 x) / (DoubleFVec2 y) = DoubleFVec2 (divideDoubleX2# x y)
>   recip (DoubleFVec2 x) = DoubleFVec2 (divideDoubleX2# (broadcastDoubleX2# 1.0##) x)
>   fromRational x = DoubleFVec2 (broadcastDoubleX2# f)
>     where (D# f) = fromRational x

>instance Num (SIMDVec 2 Int64) where
>  (Int64SVec2 x) + (Int64SVec2 y) = Int64SVec2 (plusInt64X2# x y)
>  (Int64SVec2 x) - (Int64SVec2 y) = Int64SVec2 (minusInt64X2# x y)
>  (Int64SVec2 x) * (Int64SVec2 y) = Int64SVec2 (timesInt64X2# x y)
>  abs = mapSVec2 abs
>  signum = mapSVec2 signum
>  fromInteger i = Int64SVec2 (broadcastInt64X2# ii)
>    where I64# ii = fromInteger i

>instance Num (SIMDVec 4 Int32) where
>  (Int32SVec4 x) + (Int32SVec4 y) = Int32SVec4 (plusInt32X4# x y)
>  (Int32SVec4 x) - (Int32SVec4 y) = Int32SVec4 (minusInt32X4# x y)
>  (Int32SVec4 x) * (Int32SVec4 y) = Int32SVec4 (timesInt32X4# x y)
>  abs = mapSVec4 abs
>  signum = mapSVec4 signum
>  fromInteger i = Int32SVec4 (broadcastInt32X4# ii)
>    where I32# ii = fromInteger i

>instance Num (SIMDVec 8 Int16) where
>  (Int16SVec8 x) + (Int16SVec8 y) = Int16SVec8 (plusInt16X8# x y)
>  (Int16SVec8 x) - (Int16SVec8 y) = Int16SVec8 (minusInt16X8# x y)
>  (Int16SVec8 x) * (Int16SVec8 y) = Int16SVec8 (timesInt16X8# x y)
>  abs = mapSVec8 abs
>  signum = mapSVec8 signum
>  fromInteger i = Int16SVec8 (broadcastInt16X8# ii)
>    where (I16# ii) = fromInteger i
>instance Num (SIMDVec 16 Int8) where
>  (Int8SVec16 x) + (Int8SVec16 y) = Int8SVec16 (plusInt8X16# x y)
>  (Int8SVec16 x) - (Int8SVec16 y) = Int8SVec16 (minusInt8X16# x y)
>  (Int8SVec16 x) * (Int8SVec16 y) = Int8SVec16 (timesInt8X16# x y)
>  abs = mapSVec16 abs
>  signum = mapSVec16 signum
>  fromInteger i = Int8SVec16 (broadcastInt8X16# ii)
>    where (I8# ii) = fromInteger i

>-- | <https://en.wikipedia.org/wiki/Complex_number>
>instance Num (SIMDVec 2 (Complex Float)) where
>  (ComplexVec2 x) + (ComplexVec2 y) = ComplexVec2 (plusFloatX4# x y)
>  (ComplexVec2 x) - (ComplexVec2 y) = ComplexVec2 (minusFloatX4# x y)
>  (ComplexVec2 x) * (ComplexVec2 y) = ComplexVec2 res
>    where (# x1,x2,x3,x4 #) = unpackFloatX4# x
>          (# y1,y2,y3,y4 #) = unpackFloatX4# y
>          realres1 = minusFloat# (timesFloat# x1 y1) (timesFloat# x2 y2)
>          realres2 = minusFloat# (timesFloat# x3 y3) (timesFloat# x4 y4)
>          imagres1 = plusFloat# (timesFloat# x1 y2) (timesFloat# x2 y1)
>          imagres2 = plusFloat# (timesFloat# x3 y4) (timesFloat# x4 y3)
>          res = packFloatX4# (# realres1,imagres1,realres2,imagres2 #)
>  abs = mapCVec2 abs
>  signum = mapCVec2 signum
>  fromInteger i = let F# d = fromInteger i
>                   in ComplexVec2 (packFloatX4# (# d,0.0#,d,0.0# #) )

>instance Num (SIMDVec 1 (Complex Double)) where
>  (ComplexVec1 x) + (ComplexVec1 y) = ComplexVec1 (plusDoubleX2# x y)
>  (ComplexVec1 x) - (ComplexVec1 y) = ComplexVec1 (minusDoubleX2# x y)
>  (ComplexVec1 x) * (ComplexVec1 y) = ComplexVec1 (packDoubleX2# (# rr,ri #) )
>    where (# x1,x2 #) = unpackDoubleX2# x
>          (# y1,y2 #) = unpackDoubleX2# y
>          rr = (x1 *## y1) -## (x2 *## y2)
>          ri = (x1 *## y2) +## (x2 *## y1)
>  abs = mapCVec1 abs
>  signum = mapCVec1 signum
>  fromInteger i = let D# d = fromInteger i
>                   in ComplexVec1 (packDoubleX2# (# d,0.0## #) )

>eqSVec2 :: SIMDVec 2 Int64 -> SIMDVec 2 Int64 -> Bool
>eqSVec2 (Int64SVec2 x) (Int64SVec2 y) = case unpackInt64X2# (minusInt64X2# x y) of
>    (# a,b #) -> (I64# a) == 0 && (I64# b) == 0

>eqSVec4 :: SIMDVec 4 Int32 -> SIMDVec 4 Int32 -> Bool
>eqSVec4 (Int32SVec4 x) (Int32SVec4 y) = case unpackInt32X4# (minusInt32X4# x y) of
>    (# a,b,c,d #) -> (I32# a) == 0 && (I32# b) == 0 && (I32# c) == 0 && (I32# d) == 0
>eqSVec8 :: SIMDVec 8 Int16 -> SIMDVec 8 Int16 -> Bool
>eqSVec8 (Int16SVec8 x) (Int16SVec8 y) = case unpackInt16X8# (minusInt16X8# x y) of
>    (# a,b,c,d,e,f,g,h #) -> (I16# a) == 0 && (I16# b) == 0 && (I16# c) == 0 && (I16# d) == 0 &&
>                             (I16# e) == 0 && (I16# f) == 0 && (I16# g) == 0 && (I16# h) == 0
>eqSVec16 :: SIMDVec 16 Int8 -> SIMDVec 16 Int8 -> Bool
>eqSVec16 (Int8SVec16 x) (Int8SVec16 y) = case unpackInt8X16# (minusInt8X16# x y) of
>     (# a,b,c,d,e,f,g,h, aa,bb,cc,dd,ee,ff,gg,hh #) ->
>         (I8# a) == 0 && (I8# b) == 0 && (I8# c) == 0 && (I8# d) == 0 &&
>         (I8# e) == 0 && (I8# f) == 0 && (I8# g) == 0 && (I8# h) == 0 &&
>         (I8# aa) == 0 && (I8# bb) == 0 && (I8# cc) == 0 && (I8# dd) == 0 &&
>         (I8# ee) == 0 && (I8# ff) == 0 && (I8# gg) == 0 && (I8# hh) == 0

>zipFVec2 :: (Double -> Double -> Double) -> SIMDVec 2 Double -> SIMDVec 2 Double -> SIMDVec 2 Double
>zipFVec2 f (DoubleFVec2 x) (DoubleFVec2 y) = let
>    (# a,b #) = unpackDoubleX2# x
>    (# a',b' #) = unpackDoubleX2# y
>    D# a'' = f (D# a) (D# a')
>    D# b'' = f (D# b) (D# b')
>   in DoubleFVec2 (packDoubleX2# (# a'', b'' #))

>zipFVec4 :: (Float -> Float -> Float) -> SIMDVec 4 Float -> SIMDVec 4 Float -> SIMDVec 4 Float
>zipFVec4 f (FloatFVec4 x) (FloatFVec4 y) = let
>   (# a,b,c,d #) = unpackFloatX4# x
>   (# a',b',c',d' #) = unpackFloatX4# y
>   F# a'' = f (F# a) (F# a')
>   F# b'' = f (F# b) (F# b')
>   F# c'' = f (F# c) (F# c')
>   F# d'' = f (F# d) (F# d')
>  in FloatFVec4 (packFloatX4# (# a'',b'',c'',d'' #))

>zipSVec2 :: (Int64 -> Int64 -> Int64) -> SIMDVec 2 Int64 -> SIMDVec 2 Int64 -> SIMDVec 2 Int64
>zipSVec2 f (Int64SVec2 x) (Int64SVec2 y) = let
>   (# a,b #) = unpackInt64X2# x
>   (# a',b' #) = unpackInt64X2# y
>   I64# ra = f (I64# a) (I64# a')
>   I64# rb = f (I64# b) (I64# b')
> in Int64SVec2 (packInt64X2# (# ra,rb #))

>zipSVec4 :: (Int32 -> Int32 -> Int32) -> SIMDVec 4 Int32 -> SIMDVec 4 Int32 -> SIMDVec 4 Int32
>zipSVec4 f (Int32SVec4 x) (Int32SVec4 y) = let
>   (# a,b,c,d #) = unpackInt32X4# x
>   (# a',b',c',d' #) = unpackInt32X4# y
>   I32# ra = f (I32# a) (I32# a')
>   I32# rb = f (I32# b) (I32# b')
>   I32# rc = f (I32# c) (I32# c')
>   I32# rd = f (I32# d) (I32# d')
> in Int32SVec4 (packInt32X4# (# ra,rb,rc,rd #))

>transposeFloatVector4 :: SIMDVec 4 Float -> SIMDVec 4 Float
>transposeFloatVector4 (FloatFVec4 x) = let
>    (# a,b,c,d #) = unpackFloatX4# x
>   in FloatFVec4 (packFloatX4# (# a,c,b,d #))

>fromDoubleVector2 :: Vector2 Double -> SIMDVec 2 Double
>fromDoubleVector2 (Vector2 (D# a) (D# b)) = DoubleFVec2 (packDoubleX2# (# a,b #))
>
>toDoubleVector2 :: SIMDVec 2 Double -> Vector2 Double
>toDoubleVector2 (DoubleFVec2 x) = let (# a,b #) = unpackDoubleX2# x
>                                   in Vector2 (D# a) (D# b)

>fromComplexVector2 :: Vector2 (Complex Float) -> SIMDVec 2 (Complex Float)
>fromComplexVector2 (Vector2 (F# a :+ F# b) (F# c :+ F# d)) = ComplexVec2 (packFloatX4# (# a,b,c,d #) )
>
>toComplexVector2 :: SIMDVec 2 (Complex Float) -> Vector2 (Complex Float)
>toComplexVector2 (ComplexVec2 x) = Vector2 (F# a :+ F# b) (F# c :+ F# d)
>     where (# a,b,c,d #) = unpackFloatX4# x

>fromComplexVector1 :: Complex Double -> SIMDVec 1 (Complex Double)
>fromComplexVector1 (D# x :+ D# y) = ComplexVec1 (packDoubleX2# (# x,y #))

>toComplexVector1 :: SIMDVec 1 (Complex Double) -> Complex Double
>toComplexVector1 (ComplexVec1 x) = let (# a,b #) = unpackDoubleX2# x in D# a :+ D# b

>{-# INLINE fromFloatVector4 #-}
>fromFloatVector4 :: Vector4 Float -> SIMDVec 4 Float
>fromFloatVector4 (Vector4 (F# a) (F# b) (F# c) (F# d))
> = FloatFVec4 (packFloatX4# (# a,b,c,d #))

>{-# INLINE fromFloatVector3 #-}
>fromFloatVector3 :: Vector3 Float -> SIMDVec 4 Float
>fromFloatVector3 (Vector3 (F# a) (F# b) (F# c))
> = FloatFVec4 (packFloatX4# (# a,b,c, 0.0# #))
>
>{-# INLINE toFloatVector4 #-}
>toFloatVector4 :: SIMDVec 4 Float -> Vector4 Float
>toFloatVector4 (FloatFVec4 x) = let
>    (# a,b,c,d #) = unpackFloatX4# x
>  in Vector4 (F# a) (F# b) (F# c) (F# d)

>{-# INLINE toFloatVector3 #-}
>toFloatVector3 :: SIMDVec 4 Float -> Vector3 Float
>toFloatVector3 (FloatFVec4 x) = let
>    (# a,b,c,d #) = unpackFloatX4# x
>  in Vector3 (F# a) (F# b) (F# c)


>from2x2FloatVector4 :: (Vector2 :*: Vector2) Float -> SIMDVec 4 Float
>from2x2FloatVector4 (Matrix (Vector2 (Vector2 (F# a) (F# b)) (Vector2 (F# a') (F# b'))))
> = FloatFVec4 (packFloatX4# (# a,b,a',b' #))

>to2x2FloatVector4 :: SIMDVec 4 Float -> (Vector2 :*: Vector2) Float
>to2x2FloatVector4 (FloatFVec4 x) = let (# a,b,c,d #) = unpackFloatX4# x
>            in Matrix (Vector2 (Vector2 (F# a) (F# b))
>                               (Vector2 (F# c) (F# d)))

>-- | convert a Vector4 to SIMD optimized SVec4
>{-# INLINE fromInt64Vector2 #-}
>fromInt64Vector2 :: Vector2 Int64 -> SIMDVec 2 Int64
>fromInt64Vector2 (Vector2 (I64# a) (I64# b))
> = Int64SVec2 (packInt64X2# (# a,b #))

>{-# INLINE fromInt32Vector4 #-}
>fromInt32Vector4 :: Vector4 Int32 -> SIMDVec 4 Int32
>fromInt32Vector4 (Vector4 (I32# a) (I32# b) (I32# c) (I32# d))
> = Int32SVec4 (packInt32X4# (# a,b,c,d #))

>{-# INLINE fromInt16Vector8 #-}
>fromInt16Vector8 :: Vector4 Int16 -> Vector4 Int16 -> SIMDVec 8 Int16
>fromInt16Vector8 (Vector4 (I16# a) (I16# b) (I16# c) (I16# d))
>            (Vector4 (I16# a') (I16# b') (I16# c') (I16# d'))
>   = Int16SVec8 (packInt16X8# (# a,b,c,d,a',b',c',d' #))
>
>{-# INLINE toInt16Vector8 #-}
>toInt16Vector8 :: SIMDVec 8 Int16 -> (Vector4 Int16, Vector4 Int16)
>toInt16Vector8 (Int16SVec8 x) = let (# a,b,c,d,a',b',c',d' #) = unpackInt16X8# x
>   in (Vector4 (I16# a) (I16# b) (I16# c) (I16# d),
>       Vector4 (I16# a) (I16# b) (I16# c) (I16# d))

>-- | convert a SIMD optimized SVec4 Int32 to a Vector4 Int32
>{-# INLINE toInt32Vector4 #-}
>toInt32Vector4 :: SIMDVec 4 Int32 -> Vector4 Int32
>toInt32Vector4 (Int32SVec4 x) = let (# a,b,c,d #) = unpackInt32X4# x
>                           in Vector4 (I32# a) (I32# b) (I32# c) (I32# d)

>{-# INLINE toInt64Vector2 #-}
>toInt64Vector2 :: SIMDVec 2 Int64 -> Vector2 Int64
>toInt64Vector2 (Int64SVec2 x) = let (# a,b #) = unpackInt64X2# x
>                           in Vector2 (I64# a) (I64# b)

>{-# INLINE from2x2Matrix #-}
>from2x2Matrix :: (Vector2 :*: Vector2) Int32 -> SIMDVec 4 Int32
>from2x2Matrix (Matrix (Vector2 (Vector2 (I32# a) (I32# b))
>                               (Vector2 (I32# c) (I32# d))))
>   = Int32SVec4 (packInt32X4# (# a,b,c,d #))

>{-# INLINE to2x2Matrix #-}
>-- | convert a SIMD optimized SVec4 Int32 to a 2x2 matrix
>to2x2Matrix :: SIMDVec 4 Int32 -> (Vector2 :*: Vector2) Int32
>to2x2Matrix (Int32SVec4 x) = let (# a,b,c,d #) = unpackInt32X4# x
>                              in Matrix (Vector2 (Vector2 (I32# a) (I32# b))
>                                                 (Vector2 (I32# c) (I32# d)))

>-- | convert a SIMD optimized SVec16 Int8 to a 4x4 matrix
>{-# INLINE fromInt8Vector16 #-}
>fromInt8Vector16 :: SIMDVec 16 Int8 -> (Vector4 :*: Vector4) Int8
>fromInt8Vector16 (Int8SVec16 v) = let
>   (# a1,b1,c1,d1,a2,b2,c2,d2,a3,b3,c3,d3,a4,b4,c4,d4 #) = unpackInt8X16# v
> in Matrix $ Vector4 (Vector4 (I8# a1) (I8# b1) (I8# c1) (I8# d1))
>                     (Vector4 (I8# a2) (I8# b2) (I8# c2) (I8# d2))
>                     (Vector4 (I8# a3) (I8# b3) (I8# c3) (I8# d3))
>                     (Vector4 (I8# a4) (I8# b4) (I8# c4) (I8# d4))

>-- | convert a 4x4 matrix of Int8 to SIMD optimized SVec16 Int8
>{-# INLINE toInt8Vector16 #-}
>toInt8Vector16 :: (Vector4 :*: Vector4) Int8 -> SIMDVec 16 Int8
>toInt8Vector16 (Matrix (Vector4
>                    (Vector4 (I8# a1) (I8# b1) (I8# c1) (I8# d1))
>                    (Vector4 (I8# a2) (I8# b2) (I8# c2) (I8# d2))
>                    (Vector4 (I8# a3) (I8# b3) (I8# c3) (I8# d3))
>                    (Vector4 (I8# a4) (I8# b4) (I8# c4) (I8# d4)))) =
>          Int8SVec16 (packInt8X16# (# a1,b1,c1,d1,a2,b2,c2,d2,a3,b3,c3,d3,a4,b4,c4,d4 #))
>           
>zipSVec8 :: (Int16 -> Int16 -> Int16) -> SIMDVec 8 Int16 -> SIMDVec 8 Int16 -> SIMDVec 8 Int16
>zipSVec8 fu (Int16SVec8 x) (Int16SVec8 y) = let
>   (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x
>   (# a',b',c',d',e',f',g',h' #) = unpackInt16X8# y
>   I16# ra = fu (I16# a) (I16# a')
>   I16# rb = fu (I16# b) (I16# b')
>   I16# rc = fu (I16# c) (I16# c')
>   I16# rd = fu (I16# d) (I16# d')
>   I16# re = fu (I16# e) (I16# e')
>   I16# rf = fu (I16# f) (I16# f')
>   I16# rg = fu (I16# g) (I16# g')
>   I16# rh = fu (I16# h) (I16# h')
>  in Int16SVec8 (packInt16X8# (# ra,rb,rc,rd,re,rf,rg,rh #))

>zipSVec16 :: (Int8 -> Int8 -> Int8) -> SIMDVec 16 Int8 -> SIMDVec 16 Int8 -> SIMDVec 16 Int8
>zipSVec16 fu (Int8SVec16 x) (Int8SVec16 y) = let
>   (# a,b,c,d,e,f,g,h, aa,bb,cc,dd,ee,ff,gg,hh #) = unpackInt8X16# x
>   (# a',b',c',d',e',f',g',h', aa',bb',cc',dd',ee',ff',gg',hh' #) = unpackInt8X16# y
>   I8# ra = fu (I8# a) (I8# a')
>   I8# rb = fu (I8# b) (I8# b')
>   I8# rc = fu (I8# c) (I8# c')
>   I8# rd = fu (I8# d) (I8# d')
>   I8# re = fu (I8# e) (I8# e')
>   I8# rf = fu (I8# f) (I8# f')
>   I8# rg = fu (I8# g) (I8# g')
>   I8# rh = fu (I8# h) (I8# h')
>   I8# raa = fu (I8# aa) (I8# aa')
>   I8# rbb = fu (I8# bb) (I8# bb')
>   I8# rcc = fu (I8# cc) (I8# cc')
>   I8# rdd = fu (I8# dd) (I8# dd')
>   I8# ree = fu (I8# ee) (I8# ee')
>   I8# rff = fu (I8# ff) (I8# ff')
>   I8# rgg = fu (I8# gg) (I8# gg')
>   I8# rhh = fu (I8# hh) (I8# hh')
> in Int8SVec16 (packInt8X16# (# ra,rb,rc,rd,re,rf,rg,rh,raa,rbb,rcc,rdd,ree,rff,rgg,rhh #))
>
>zipCVec2 :: (Complex Float -> Complex Float -> Complex Float) -> SIMDVec 2 (Complex Float) -> SIMDVec 2 (Complex Float) -> SIMDVec 2 (Complex Float)
>zipCVec2 f (ComplexVec2 x) (ComplexVec2 y) = ComplexVec2 res
>   where (# a,b,c,d #) = unpackFloatX4# x
>         (# a',b',c',d' #) = unpackFloatX4# y
>         F# ra :+ F# rb = f (F# a :+ F# b) (F# a' :+ F# b')
>         F# rc :+ F# rd = f (F# c :+ F# d) (F# c' :+ F# d')
>         res = packFloatX4# (# ra,rb,rc,rd #)

>zipCVec2_components :: (Float -> Float -> Float) ->
>        SIMDVec 2 (Complex Float) -> SIMDVec 2 (Complex Float) -> SIMDVec 2 (Complex Float)
>zipCVec2_components f (ComplexVec2 x) (ComplexVec2 y) = ComplexVec2 res
>   where (# a,b,c,d #) = unpackFloatX4# x
>         (# a',b',c',d' #) = unpackFloatX4# y
>         F# resa = f (F# a) (F# a')
>         F# resb = f (F# b) (F# b')
>         F# resc = f (F# c) (F# c')
>         F# resd = f (F# d) (F# d')
>         res = packFloatX4# (# resa, resb, resc, resd #)

>zipCVec1 :: (Complex Double -> Complex Double -> Complex Double) ->
>        SIMDVec 1 (Complex Double) -> SIMDVec 1 (Complex Double) -> SIMDVec 1 (Complex Double)
>zipCVec1 f (ComplexVec1 x) (ComplexVec1 y) = ComplexVec1 res
>   where (# a,b #) = unpackDoubleX2# x
>         (# a',b' #) = unpackDoubleX2# y
>         (D# resa :+ D# resb) = f (D# a :+ D# b) (D# a' :+ D# b')
>         res = packDoubleX2# (# resa, resb #)
>
>zipCVec1_components :: (Double -> Double -> Double) ->
>        SIMDVec 1 (Complex Double) -> SIMDVec 1 (Complex Double) -> SIMDVec 1 (Complex Double)
>zipCVec1_components f (ComplexVec1 x) (ComplexVec1 y) = ComplexVec1 res
>   where (# a,b #) = unpackDoubleX2# x
>         (# a',b' #) = unpackDoubleX2# y
>         D# resa = f (D# a) (D# a')
>         D# resb = f (D# b) (D# b')
>         res = packDoubleX2# (# resa, resb #)
> 
>{-# INLINE mapSVec2 #-}
>mapSVec2 :: (Int64 -> Int64) -> SIMDVec 2 Int64 -> SIMDVec 2 Int64
>mapSVec2 f (Int64SVec2 x) = let (# a,b #) = unpackInt64X2# x
>                                I64# fa = f (I64# a)
>                                I64# fb = f (I64# b)
>                             in Int64SVec2 (packInt64X2# (# fa,fb #))

>{-# INLINE mapSVec4 #-}
>mapSVec4 :: (Int32 -> Int32) -> SIMDVec 4 Int32 -> SIMDVec 4 Int32
>mapSVec4 f (Int32SVec4 x) = let (# a,b,c,d #) = unpackInt32X4# x
>                                I32# fa = f (I32# a)
>                                I32# fb = f (I32# b)
>                                I32# fc = f (I32# c)
>                                I32# fd = f (I32# d)
>                             in Int32SVec4 (packInt32X4# (# fa,fb,fc,fd #))

>{-# INLINE mapFVec2 #-}
>mapFVec2 :: (Double -> Double) -> SIMDVec 2 Double -> SIMDVec 2 Double
>mapFVec2 f (DoubleFVec2 x) = let (# a,b #) = unpackDoubleX2# x
>                                 D# a' = f (D# a)
>                                 D# b' = f (D# b)
>                              in DoubleFVec2 (packDoubleX2# (# a', b' #))

>{-# INLINE mapFVec4 #-}
>mapFVec4 :: (Float -> Float) -> SIMDVec 4 Float -> SIMDVec 4 Float
>mapFVec4 f (FloatFVec4 x) = let (# a,b,c,d #) = unpackFloatX4# x
>                                F# a' = f (F# a)
>                                F# b' = f (F# b)
>                                F# c' = f (F# c)
>                                F# d' = f (F# d)
>                              in FloatFVec4 (packFloatX4# (# a',b',c',d' #))
>                             

>mapCVec2 :: (Complex Float -> Complex Float) -> SIMDVec 2 (Complex Float) -> SIMDVec 2 (Complex Float)
>mapCVec2 f (ComplexVec2 x) = let (# a,b,c,d #) = unpackFloatX4# x
>                                 F# a' :+ F# b' = f (F# a :+ F# b)
>                                 F# c' :+ F# d' = f (F# c :+ F# d)
>                              in ComplexVec2 (packFloatX4# (# a',b',c',d' #))
> 
>mapCVec1 :: (Complex Double -> Complex Double) -> SIMDVec 1 (Complex Double) -> SIMDVec 1 (Complex Double)
>mapCVec1 f (ComplexVec1 x) = let (# a,b #) = unpackDoubleX2# x
>                                 D# a' :+ D# b' = f (D# a :+ D# b)
>                              in ComplexVec1 (packDoubleX2# (# a',b' #))

>mapCVec1_components :: (Double -> Double) -> SIMDVec 1 (Complex Double) -> SIMDVec 1 (Complex Double)
>mapCVec1_components f (ComplexVec1 x) = ComplexVec1 res
>    where (# a,b #) = unpackDoubleX2# x
>          D# a' = f (D# a)
>          D# b' = f (D# b)
>          res = packDoubleX2# (# a', b' #)


>{-# INLINE fvec4_x #-}
>{-# INLINE fvec4_y #-}
>{-# INLINE fvec4_z #-}
>{-# INLINE fvec4_t #-}
>fvec4_x,fvec4_y,fvec4_z,fvec4_t :: SIMDVec 4 Float -> Float
>fvec4_x (FloatFVec4 x) = let (# a,_,_,_ #) = unpackFloatX4# x in F# a
>fvec4_y (FloatFVec4 x) = let (# _,b,_,_ #) = unpackFloatX4# x in F# b
>fvec4_z (FloatFVec4 x) = let (# _,_,c,_ #) = unpackFloatX4# x in F# c
>fvec4_t (FloatFVec4 x) = let (# _,_,_,d #) = unpackFloatX4# x in F# d

>{-# INLINE svec2_x #-}
>svec2_x :: SIMDVec 2 Int64 -> Int64
>svec2_x (Int64SVec2 x) = let (# a,_ #) = unpackInt64X2# x in I64# a

>{-# INLINE svec2_y #-}
>svec2_y :: SIMDVec 2 Int64 -> Int64
>svec2_y (Int64SVec2 x) = let (# _,b #) = unpackInt64X2# x in I64# b

>{-# INLINE svec4_x #-}
>svec4_x :: SIMDVec 4 Int32 -> Int32
>svec4_x (Int32SVec4 x# ) = let (# a,_,_,_ #) = unpackInt32X4# x# in I32# a
>
>{-# INLINE svec4_y #-}
>svec4_y :: SIMDVec 4 Int32 -> Int32
>svec4_y (Int32SVec4 x# ) = let (# _,b,_,_ #) = unpackInt32X4# x# in I32# b

>{-# INLINE svec4_z #-}
>svec4_z ::SIMDVec 4 Int32 -> Int32
>svec4_z (Int32SVec4 x# ) = let (# _,_,c,_ #) = unpackInt32X4# x# in I32# c

>{-# INLINE svec4_t #-}
>svec4_t :: SIMDVec 4 Int32 -> Int32
>svec4_t (Int32SVec4 x# ) = let (# _,_,_,d #) = unpackInt32X4# x# in I32# d

>mapSVec8 :: (Int16 -> Int16) -> SIMDVec 8 Int16 -> SIMDVec 8 Int16
>mapSVec8 func (Int16SVec8 x) = let (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x
>                                   I16# fa = func (I16# a)
>                                   I16# fb = func (I16# b)
>                                   I16# fc = func (I16# c)
>                                   I16# fd = func (I16# d)
>                                   I16# fe = func (I16# e)
>                                   I16# ff = func (I16# f)
>                                   I16# fg = func (I16# g)
>                                   I16# fh = func (I16# h)
>                             in Int16SVec8 (packInt16X8# (# fa,fb,fc,fd,fe,ff,fg,fh #))


>svec8_1 :: SIMDVec 8 Int16 -> Int16
>svec8_2 :: SIMDVec 8 Int16 -> Int16
>svec8_3 :: SIMDVec 8 Int16 -> Int16
>svec8_4 :: SIMDVec 8 Int16 -> Int16
>svec8_5 :: SIMDVec 8 Int16 -> Int16
>svec8_6 :: SIMDVec 8 Int16 -> Int16
>svec8_7 :: SIMDVec 8 Int16 -> Int16
>svec8_8 :: SIMDVec 8 Int16 -> Int16
>svec8_1 (Int16SVec8 x# ) = let (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x# in I16# a
>svec8_2 (Int16SVec8 x# ) = let (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x# in I16# b
>svec8_3 (Int16SVec8 x# ) = let (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x# in I16# c
>svec8_4 (Int16SVec8 x# ) = let (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x# in I16# d
>svec8_5 (Int16SVec8 x# ) = let (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x# in I16# e
>svec8_6 (Int16SVec8 x# ) = let (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x# in I16# f
>svec8_7 (Int16SVec8 x# ) = let (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x# in I16# g
>svec8_8 (Int16SVec8 x# ) = let (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x# in I16# h


>svec16_1 :: SIMDVec 16 Int8 -> Int8
>svec16_2 :: SIMDVec 16 Int8 -> Int8
>svec16_3 :: SIMDVec 16 Int8 -> Int8
>svec16_4 :: SIMDVec 16 Int8 -> Int8
>svec16_5 :: SIMDVec 16 Int8 -> Int8
>svec16_6 :: SIMDVec 16 Int8 -> Int8
>svec16_7 :: SIMDVec 16 Int8 -> Int8
>svec16_8 :: SIMDVec 16 Int8 -> Int8
>svec16_9 :: SIMDVec 16 Int8 -> Int8
>svec16_10 :: SIMDVec 16 Int8 -> Int8
>svec16_11 :: SIMDVec 16 Int8 -> Int8
>svec16_12 :: SIMDVec 16 Int8 -> Int8
>svec16_13 :: SIMDVec 16 Int8 -> Int8
>svec16_14 :: SIMDVec 16 Int8 -> Int8
>svec16_15 :: SIMDVec 16 Int8 -> Int8
>svec16_16 :: SIMDVec 16 Int8 -> Int8
>svec16_1 (Int8SVec16 x# ) = let (# a,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ #) = unpackInt8X16# x# in I8# a
>svec16_2 (Int8SVec16 x# ) = let (# _,b,_,_,_,_,_,_,_,_,_,_,_,_,_,_ #) = unpackInt8X16# x# in I8# b
>svec16_3 (Int8SVec16 x# ) = let (# _,_,c,_,_,_,_,_,_,_,_,_,_,_,_,_ #) = unpackInt8X16# x# in I8# c
>svec16_4 (Int8SVec16 x# ) = let (# _,_,_,d,_,_,_,_,_,_,_,_,_,_,_,_ #) = unpackInt8X16# x# in I8# d
>svec16_5 (Int8SVec16 x# ) = let (# _,_,_,_,e,_,_,_,_,_,_,_,_,_,_,_ #) = unpackInt8X16# x# in I8# e
>svec16_6 (Int8SVec16 x# ) = let (# _,_,_,_,_,f,_,_,_,_,_,_,_,_,_,_ #) = unpackInt8X16# x# in I8# f
>svec16_7 (Int8SVec16 x# ) = let (# _,_,_,_,_,_,g,_,_,_,_,_,_,_,_,_ #) = unpackInt8X16# x# in I8# g
>svec16_8 (Int8SVec16 x# ) = let (# _,_,_,_,_,_,_,h,_,_,_,_,_,_,_,_ #) = unpackInt8X16# x# in I8# h
>svec16_9 (Int8SVec16 x# ) = let (# _,_,_,_,_,_,_,_,a,_,_,_,_,_,_,_ #) = unpackInt8X16# x# in I8# a
>svec16_10 (Int8SVec16 x# ) = let (# _,_,_,_,_,_,_,_,_,b,_,_,_,_,_,_ #) = unpackInt8X16# x# in I8# b
>svec16_11 (Int8SVec16 x# ) = let (# _,_,_,_,_,_,_,_,_,_,c,_,_,_,_,_ #) = unpackInt8X16# x# in I8# c
>svec16_12 (Int8SVec16 x# ) = let (# _,_,_,_,_,_,_,_,_,_,_,d,_,_,_,_ #) = unpackInt8X16# x# in I8# d
>svec16_13 (Int8SVec16 x# ) = let (# _,_,_,_,_,_,_,_,_,_,_,_,e,_,_,_ #) = unpackInt8X16# x# in I8# e
>svec16_14 (Int8SVec16 x# ) = let (# _,_,_,_,_,_,_,_,_,_,_,_,_,f,_,_ #) = unpackInt8X16# x# in I8# f
>svec16_15 (Int8SVec16 x# ) = let (# _,_,_,_,_,_,_,_,_,_,_,_,_,_,g,_ #) = unpackInt8X16# x# in I8# g
>svec16_16 (Int8SVec16 x# ) = let (# _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,h #) = unpackInt8X16# x# in I8# h


>mapSVec16 :: (Int8  -> Int8) -> SIMDVec 16 Int8 -> SIMDVec 16 Int8
>mapSVec16 func (Int8SVec16 x) = let (# a,b,c,d,e,f,g,h,
>                                      a',b',c',d',e',f',g',h' #) = unpackInt8X16# x
>                                    I8# fa = func (I8# a)
>                                    I8# fb = func (I8# b)
>                                    I8# fc = func (I8# c)
>                                    I8# fd = func (I8# d)
>                                    I8# fe = func (I8# e)
>                                    I8# ff = func (I8# f)
>                                    I8# fg = func (I8# g)
>                                    I8# fh = func (I8# h)
>                                    I8# fa' = func (I8# a')
>                                    I8# fb' = func (I8# b')
>                                    I8# fc' = func (I8# c')
>                                    I8# fd' = func (I8# d')
>                                    I8# fe' = func (I8# e')
>                                    I8# ff' = func (I8# f')
>                                    I8# fg' = func (I8# g')
>                                    I8# fh' = func (I8# h')
>                                in Int8SVec16 (packInt8X16# (# fa,fb,fc,fd,fe,ff,fg,fh,fa',fb',fc',fd',fe',ff',fg',fh' #))

>constantFVec2 :: Double -> SIMDVec 2 Double
>constantFVec2 (D# x) = DoubleFVec2 (broadcastDoubleX2# x)

>{-# INLINE constantFVec4 #-}
>constantFVec4 :: Float -> SIMDVec 4 Float
>constantFVec4 (F# x) = FloatFVec4 (broadcastFloatX4# x)

>{-# INLINE constantSVec2 #-}
>constantSVec2 :: Int64 -> SIMDVec 2 Int64
>constantSVec2 (I64# x# ) = Int64SVec2 (broadcastInt64X2# x# )
> 
>{-# INLINE constantSVec4 #-}
>constantSVec4 :: Int32 -> SIMDVec 4 Int32
>constantSVec4 (I32# x# ) = Int32SVec4 (broadcastInt32X4# x# )

>{-# INLINE constantSVec8 #-}
>constantSVec8 :: Int16 -> SIMDVec 8 Int16
>constantSVec8 (I16# x# ) = Int16SVec8 (broadcastInt16X8# x# )
>
>{-# INLINE constantSVec16 #-}
>constantSVec16 :: Int8 -> SIMDVec 16 Int8
>constantSVec16 (I8# x# ) = Int8SVec16 (broadcastInt8X16# x# )

>constantCVec1 :: Complex Double -> SIMDVec 1 (Complex Double)
>constantCVec1 (D# a :+ D# b) = ComplexVec1 (packDoubleX2# (# a,b #) )

>constantCVec1_components :: Double -> SIMDVec 1 (Complex Double)
>constantCVec1_components (D# a) = ComplexVec1 (packDoubleX2# (# a,a #) )

>constantCVec2 :: Complex Float -> SIMDVec 2 (Complex Float)
>constantCVec2 (F# a :+ F# b) = ComplexVec2 (packFloatX4# (# a,b,a,b #) )
> 
>constantCVec2_components :: Float -> SIMDVec 2 (Complex Float)
>constantCVec2_components (F# a) = ComplexVec2 (packFloatX4# (# a,a,a,a #) )

>{-# INLINE makeFVec4 #-}
>makeFVec4 :: Float -> Float -> Float -> Float -> SIMDVec 4 Float
>makeFVec4 (F# a) (F# b) (F# c) (F# d) =
>   FloatFVec4 (packFloatX4# (# a,b,c,d #))

>{-# INLINE makeSVec2 #-}
>makeSVec2 :: Int64 -> Int64 -> SIMDVec 2 Int64
>makeSVec2 (I64# a) (I64# b)
>   = Int64SVec2 (packInt64X2# (# a, b #) )

>{-# INLINE makeSVec4 #-}
>makeSVec4 :: Int32 -> Int32 -> Int32 -> Int32 -> SIMDVec 4 Int32
>makeSVec4 (I32# a) (I32# b) (I32# c) (I32# d)
>   = Int32SVec4 (packInt32X4# (# a, b, c, d #) )


>{-# INLINE makeSVec8 #-}
>makeSVec8 :: Int16 -> Int16 -> Int16 -> Int16
>          -> Int16 -> Int16 -> Int16 -> Int16
>          -> SIMDVec 8 Int16
>makeSVec8 (I16# a) (I16# b) (I16# c) (I16# d) (I16# e) (I16# f) (I16# g) (I16# h) = Int16SVec8 (packInt16X8# (# a,b,c,d,e,f,g,h #))
>

>{-# INLINE makeSVec16 #-}
>makeSVec16 :: Int8 -> Int8 -> Int8 -> Int8
>           -> Int8 -> Int8 -> Int8 -> Int8
>           -> Int8 -> Int8 -> Int8 -> Int8
>           -> Int8 -> Int8 -> Int8 -> Int8
>           -> SIMDVec 16 Int8
>makeSVec16 (I8# a) (I8# b) (I8# c) (I8# d)
>           (I8# e) (I8# f) (I8# g) (I8# h)
>           (I8# a') (I8# b') (I8# c') (I8# d')
>           (I8# e') (I8# f') (I8# g') (I8# h')
>   = Int8SVec16 (packInt8X16# (# a,b,c,d,e,f,g,h,a',b',c',d',e',f',g',h' #))

#if defined(WITH_LLVM)

>instance VectorSpace (SIMDVec 4 Float) where
>   type Scalar (SIMDVec 4 Float) = Float
>   vzero = FloatFVec4 (broadcastFloatX4# 0.0#)
>   vnegate (FloatFVec4 x) = FloatFVec4 (negateFloatX4# x)
>   (FloatFVec4 x) %+ (FloatFVec4 y) = FloatFVec4 (plusFloatX4# x y)
>   (F# x) %* (FloatFVec4 v) = FloatFVec4 (timesFloatX4# (broadcastFloatX4# x) v)

>instance VectorSpace (SIMDVec 2 Double) where
>   type Scalar (SIMDVec 2 Double) = Double
>   vzero = DoubleFVec2 (broadcastDoubleX2# 0.0##)
>   vnegate (DoubleFVec2 x) = DoubleFVec2 (negateDoubleX2# x)
>   (DoubleFVec2 x) %+ (DoubleFVec2 y) = DoubleFVec2 (plusDoubleX2# x y)
>   (D# x) %* (DoubleFVec2 v) = DoubleFVec2 (timesDoubleX2# (broadcastDoubleX2# x) v)

>instance VectorSpace (SIMDVec 2 Int64) where
>   type Scalar (SIMDVec 2 Int64) = Int64
>   vzero = Int64SVec2 (broadcastInt64X2# (intToInt64# 0#))
>   vnegate (Int64SVec2 i) = Int64SVec2 (negateInt64X2# i)
>   (Int64SVec2 x) %+ (Int64SVec2 y) = Int64SVec2 (plusInt64X2# x y)
>   (I64# x) %* (Int64SVec2 v) = Int64SVec2 (timesInt64X2# (broadcastInt64X2# x) v)


>instance VectorSpace (SIMDVec 4 Int32) where
>   type Scalar (SIMDVec 4 Int32) = Int32
>   vzero = Int32SVec4 (broadcastInt32X4# (intToInt32# 0#))
>   vnegate (Int32SVec4 i) = Int32SVec4 (negateInt32X4# i)
>   (Int32SVec4 x) %+ (Int32SVec4 y) = Int32SVec4 (plusInt32X4# x y)
>   (I32# x) %* (Int32SVec4 v) = Int32SVec4 (timesInt32X4# (broadcastInt32X4# x) v)

>instance VectorSpace (SIMDVec 8 Int16) where
>   type Scalar (SIMDVec 8 Int16) = Int16
>   vzero = Int16SVec8 (broadcastInt16X8# (intToInt16# 0#))
>   vnegate (Int16SVec8 i) = Int16SVec8 (negateInt16X8# i)
>   (Int16SVec8 x) %+ (Int16SVec8 y) = Int16SVec8 (plusInt16X8# x y)
>   (I16# x) %* (Int16SVec8 v) = Int16SVec8 (timesInt16X8# (broadcastInt16X8# x) v)

>instance VectorSpace (SIMDVec 16 Int8) where
>   type Scalar (SIMDVec 16 Int8) = Int8
>   vzero = Int8SVec16 (broadcastInt8X16# (intToInt8# 0#))
>   vnegate (Int8SVec16 i) = Int8SVec16 (negateInt8X16# i)
>   (Int8SVec16 x) %+ (Int8SVec16 y) = Int8SVec16 (plusInt8X16# x y)
>   (I8# x) %* (Int8SVec16 v) = Int8SVec16 (timesInt8X16# (broadcastInt8X16# x) v)

>instance VectorSpace (SIMDVec 2 (Complex Float)) where
>   type Scalar (SIMDVec 2 (Complex Float)) = Complex Float
>   vzero = ComplexVec2 (broadcastFloatX4# 0.0#)
>   vnegate (ComplexVec2 x) = ComplexVec2 (negateFloatX4# x)
>   (ComplexVec2 x) %+ (ComplexVec2 y) = ComplexVec2 (plusFloatX4# x y)
>   (F# x :+ F# y) %* (ComplexVec2 v) = ComplexVec2 (timesFloatX4# (packFloatX4# (# x,y,x,y #) ) v)
> 
>instance VectorSpace (SIMDVec 1 (Complex Double)) where
>   type Scalar (SIMDVec 1 (Complex Double)) = Complex Double
>   vzero = ComplexVec1 (broadcastDoubleX2# 0.0##)
>   vnegate (ComplexVec1 x) = ComplexVec1 (negateDoubleX2# x)
>   (ComplexVec1 x) %+ (ComplexVec1 y) = ComplexVec1 (plusDoubleX2# x y)
>   (D# x :+ D# y) %* (ComplexVec1 v) = ComplexVec1 (timesDoubleX2# (packDoubleX2# (# x,y #) ) v)

#endif

#else

-- #warning "SIMD optimizations are disabled. --flag '*:LLVM' is required for SIMD optimizations."

>import GHC.Int
>import GHC.TypeLits

>data SIMDVec (len :: Nat) a where
>   SVec2 :: !Int64 -> !Int64 -> SIMDVec 2 Int64
>   SVec4 :: !Int32 -> !Int32 -> !Int32 -> !Int32 -> SIMDVec 4 Int32
>   SVec8 :: !Int16 -> !Int16 -> !Int16 -> !Int16
>         -> !Int16 -> !Int16 -> !Int16 -> !Int16
>         -> SIMDVec 8 Int16
>   SVec16 :: !Int8 -> !Int8 -> !Int8 -> !Int8
>          -> !Int8 -> !Int8 -> !Int8 -> !Int8
>          -> !Int8 -> !Int8 -> !Int8 -> !Int8
>          -> !Int8 -> !Int8 -> !Int8 -> !Int8
>          -> SIMDVec 16 Int8
>   FVec2 :: !Double -> !Double -> SIMDVec 2 Double
>   FVec4 :: !Float -> !Float -> !Float -> !Float -> SIMDVec 4 Float
>   CVec2 :: !(Complex Float) -> !(Complex Float) -> SIMDVec 2 (Complex Float)
>   CVec1 :: !(Complex Double) -> SIMDVec 1 (Complex Double)

>mapCVec2 :: (Complex Float -> Complex Float) -> SIMDVec 2 (Complex Float) -> SIMDVec 2 (Complex Float)
>mapCVec2 f (CVec2 x y) = CVec2 (f x) (f y)
>
>mapCVec2_components :: (Float -> Float) -> SIMDVec 2 (Complex Float) -> SIMDVec 2 (Complex Float)
>mapCVec2_components f (CVec2 (x :+ xi) (y :+ yi))
>  = CVec2 (f x :+ f xi) (f y :+ f yi)
>
>mapCVec1 :: (Complex Double -> Complex Double) -> SIMDVec 1 (Complex Double) -> SIMDVec 1 (Complex Double)
>mapCVec1 f (CVec1 x) = CVec1 (f x)

>mapCVec1_components :: (Double -> Double) -> SIMDVec 1 (Complex Double) -> SIMDVec 1 (Complex Double)
>mapCVec1_components f (CVec1 (x :+ xi)) = CVec1 (f x :+ f xi)

>conjSIMD2 :: SIMDVec 2 (Complex Float) -> SIMDVec 2 (Complex Float)
>conjSIMD2 (CVec2 x y) = CVec2 (conjugate x) (conjugate y)

>conjSIMD1 :: SIMDVec 1 (Complex Double) -> SIMDVec 1 (Complex Double)
>conjSIMD1 (CVec1 x) = CVec1 (conjugate x)

>fromIntegralSIMD4 :: SIMDVec 4 Int32 -> SIMDVec 4 Float
>fromIntegralSIMD4 (SVec4 a b c d) = FVec4 (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)
>fromIntegralSIMD2 :: SIMDVec 2 Int64 -> SIMDVec 2 Double
>fromIntegralSIMD2 (SVec2 a b) = FVec2 (fromIntegral a) (fromIntegral b)

>eqSVec2 :: SIMDVec 2 Int64 -> SIMDVec 2 Int64 -> Bool
>eqSVec2 (SVec2 a b ) (SVec2 a' b') = a == a' && b == b'
> 
>eqSVec4 :: SIMDVec 4 Int32 -> SIMDVec 4 Int32 -> Bool
>eqSVec4 (SVec4 a b c d) (SVec4 a' b' c' d') = a == a' && b == b' && c == c' && d == d'
>eqSVec8 :: SIMDVec 8 Int16 -> SIMDVec 8 Int16 -> Bool
>eqSVec8 (SVec8 a b c d e f g h) (SVec8 a' b' c' d' e' f' g' h') =
>    a == a' && b == b' && c == c' && d == d' &&
>    e == e' && f == f' && g == g' && h == h'
>eqSVec16 :: SIMDVec 16 Int8 -> SIMDVec 16 Int8 -> Bool
>eqSVec16 (SVec16 a b c d e f g h aa bb cc dd ee ff gg hh)
>         (SVec16 a' b' c' d' e' f' g' h' aa' bb' cc' dd' ee' ff' gg' hh') =
>              a == a' && b == b' && c == c' && d == d' &&
>              e == e' && f == f' && g == g' && h == h' &&
>              aa == aa' && bb == bb' && cc == cc' && dd == dd' &&
>              ee == ee' && ff == ff' && gg == gg' && hh == hh'

>to2x2Matrix :: SIMDVec 4 Int32 -> (Vector2 :*: Vector2) Int32
>to2x2Matrix (SVec4 a b c d) = Matrix $ Vector2 (Vector2 a b) (Vector2 c d)

>from2x2Matrix :: (Vector2 :*: Vector2) Int32 -> SIMDVec 4 Int32
>from2x2Matrix (Matrix (Vector2 (Vector2 a b) (Vector2 c d))) = SVec4 a b c d

>svec16_1 :: SIMDVec 16 Int8 -> Int8
>svec16_2 :: SIMDVec 16 Int8 -> Int8
>svec16_3 :: SIMDVec 16 Int8 -> Int8
>svec16_4 :: SIMDVec 16 Int8 -> Int8
>svec16_5 :: SIMDVec 16 Int8 -> Int8
>svec16_6 :: SIMDVec 16 Int8 -> Int8
>svec16_7 :: SIMDVec 16 Int8 -> Int8
>svec16_8 :: SIMDVec 16 Int8 -> Int8
>svec16_9 :: SIMDVec 16 Int8 -> Int8
>svec16_10 :: SIMDVec 16 Int8 -> Int8
>svec16_11 :: SIMDVec 16 Int8 -> Int8
>svec16_12 :: SIMDVec 16 Int8 -> Int8
>svec16_13 :: SIMDVec 16 Int8 -> Int8
>svec16_14 :: SIMDVec 16 Int8 -> Int8
>svec16_15 :: SIMDVec 16 Int8 -> Int8
>svec16_16 :: SIMDVec 16 Int8 -> Int8
>svec16_1 (SVec16 a _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ) = a
>svec16_2 (SVec16 _ b _ _ _ _ _ _ _ _ _ _ _ _ _ _ ) = b
>svec16_3 (SVec16 _ _ c _ _ _ _ _ _ _ _ _ _ _ _ _ ) = c
>svec16_4 (SVec16 _ _ _ d _ _ _ _ _ _ _ _ _ _ _ _ ) = d
>svec16_5 (SVec16 _ _ _ _ e _ _ _ _ _ _ _ _ _ _ _ ) = e
>svec16_6 (SVec16 _ _ _ _ _ f _ _ _ _ _ _ _ _ _ _ ) = f
>svec16_7 (SVec16 _ _ _ _ _ _ g _ _ _ _ _ _ _ _ _ ) = g
>svec16_8 (SVec16 _ _ _ _ _ _ _ h _ _ _ _ _ _ _ _ ) = h
>svec16_9 (SVec16 _ _ _ _ _ _ _ _ a _ _ _ _ _ _ _) = a
>svec16_10 (SVec16 _ _ _ _ _ _ _ _ _ b _ _ _ _ _ _) = b
>svec16_11 (SVec16 _ _ _ _ _ _ _ _ _ _ c _ _ _ _ _) = c
>svec16_12 (SVec16 _ _ _ _ _ _ _ _ _ _ _ d _ _ _ _) = d
>svec16_13 (SVec16 _ _ _ _ _ _ _ _ _ _ _ _ e _ _ _) = e
>svec16_14 (SVec16 _ _ _ _ _ _ _ _ _ _ _ _ _ f _ _) = f
>svec16_15 (SVec16 _ _ _ _ _ _ _ _ _ _ _ _ _ _ g _) = g
>svec16_16 (SVec16 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ h) = h


>mapSVec16 :: (Int8  -> Int8) -> SIMDVec 16 Int8 -> SIMDVec 16 Int8
>mapSVec16 fu (SVec16 a b c d e f g h a' b' c' d' e' f' g' h')
>  = SVec16 (fu a) (fu b) (fu c) (fu d) (fu e) (fu f) (fu g) (fu h)
>          (fu a') (fu b') (fu c') (fu d') (fu e') (fu f') (fu g') (fu h')
>mapSVec8 :: (Int16 -> Int16) -> SIMDVec 8 Int16 -> SIMDVec 8 Int16
>mapSVec8 fu (SVec8 a b c d e f g h)
>   = SVec8 (fu a) (fu b) (fu c) (fu d) (fu e) (fu f) (fu g) (fu h)

>mapSVec4 :: (Int32 -> Int32) -> SIMDVec 4 Int32 -> SIMDVec 4 Int32
>mapSVec4 fu (SVec4 a b c d) = SVec4 (fu a) (fu b) (fu c) (fu d)

>mapSVec2 :: (Int64 -> Int64) -> SIMDVec 2 Int64 -> SIMDVec 2 Int64
>mapSVec2 fu (SVec2 a b) = SVec2 (fu a) (fu b) 

>mapFVec4 :: (Float -> Float) -> SIMDVec 4 Float -> SIMDVec 4 Float
>mapFVec4 fu (FVec4 a b c d) = FVec4 (fu a) (fu b) (fu c) (fu d)

>mapFVec2 :: (Double -> Double) -> SIMDVec 2 Double -> SIMDVec 2 Double
>mapFVec2 fu (FVec2 a b) = FVec2 (fu a) (fu b)

>zipSVec2 :: (Int64 -> Int64 -> Int64) -> SIMDVec 2 Int64 -> SIMDVec 2 Int64 -> SIMDVec 2 Int64
>zipSVec2 fu (SVec2 a b) (SVec2 a' b')
>  = SVec2 (fu a a') (fu b b')

>zipSVec4 :: (Int32 -> Int32 -> Int32) -> SIMDVec 4 Int32 -> SIMDVec 4 Int32 -> SIMDVec 4 Int32
>zipSVec4 fu (SVec4 a b c d) (SVec4 a' b' c' d')
>  = SVec4 (fu a a') (fu b b') (fu c c') (fu d d')

>zipFVec2 :: (Double -> Double -> Double) -> SIMDVec 2 Double -> SIMDVec 2 Double -> SIMDVec 2 Double
>zipFVec2 fu (FVec2 a b) (FVec2 a' b') = FVec2 (fu a a') (fu b b')

>zipFVec4 :: (Float -> Float -> Float) -> SIMDVec 4 Float -> SIMDVec 4 Float -> SIMDVec 4 Float
>zipFVec4 fu (FVec4 a b c d) (FVec4 a' b' c' d')
>  = FVec4 (fu a a') (fu b b') (fu c c') (fu d d')

>zipSVec8 :: (Int16 -> Int16 -> Int16) -> SIMDVec 8 Int16 -> SIMDVec 8 Int16 -> SIMDVec 8 Int16
>zipSVec8 fu (SVec8 a b c d e f g h) (SVec8 a' b' c' d' e' f' g' h')
>  = SVec8 (fu a a') (fu b b') (fu c c') (fu d d')
>          (fu e e') (fu f f') (fu g g') (fu h h')

>zipSVec16 :: (Int8 -> Int8 -> Int8) -> SIMDVec 16 Int8 -> SIMDVec 16 Int8 -> SIMDVec 16 Int8
>zipSVec16 fu (SVec16 a b c d e f g h aa bb cc dd ee ff gg hh)
>             (SVec16 a' b' c' d' e' f' g' h' aa' bb' cc' dd' ee' ff' gg' hh')
>  = SVec16 (fu a a') (fu b b') (fu c c') (fu d d')
>          (fu e e') (fu f f') (fu g g') (fu h h')
>          (fu aa aa') (fu bb bb') (fu cc cc') (fu dd dd')
>          (fu ee ee') (fu ff ff') (fu gg gg') (fu hh hh')

>zipCVec2 :: (Complex Float -> Complex Float -> Complex Float)
>     -> SIMDVec 2 (Complex Float) -> SIMDVec 2 (Complex Float) -> SIMDVec 2 (Complex Float)
>zipCVec2 f (CVec2 a b) (CVec2 a' b') = CVec2 (f a a') (f b b')

>zipCVec2_components :: (Float -> Float -> Float)
>    -> SIMDVec 2 (Complex Float) -> SIMDVec 2 (Complex Float) -> SIMDVec 2 (Complex Float)
>zipCVec2_components f (CVec2 (a :+ ai) (a' :+ ai'))
>                      (CVec2 (b :+ bi) (b' :+ bi')) =
>   CVec2 (f a b :+ f ai bi) (f a' b' :+ f ai' bi')

>zipCVec1 :: (Complex Double -> Complex Double -> Complex Double)
>   -> SIMDVec 1 (Complex Double) -> SIMDVec 1 (Complex Double) -> SIMDVec 1 (Complex Double)
>zipCVec1 f (CVec1 a) (CVec1 b) = CVec1 (f a b)
>
>zipCVec1_components :: (Double -> Double -> Double)
>    -> SIMDVec 1 (Complex Double) -> SIMDVec 1 (Complex Double) -> SIMDVec 1 (Complex Double)
>zipCVec1_components f (CVec1 (a :+ ai)) (CVec1 (b :+ bi)) =
>   CVec1 (f a b :+ f ai bi)
>
>constantFVec2 :: Double -> SIMDVec 2 Double
>constantFVec2 a = FVec2 a a


>constantFVec4 :: Float -> SIMDVec 4 Float
>constantFVec4 a = FVec4 a a a a

>constantSVec2 :: Int64 -> SIMDVec 2 Int64
>constantSVec2 a = SVec2 a a 

>constantSVec4 :: Int32 -> SIMDVec 4 Int32
>constantSVec4 a = SVec4 a a a a

>constantSVec8 :: Int16 -> SIMDVec 8 Int16
>constantSVec8 a = SVec8 a a a a a a a a

>constantSVec16 :: Int8 -> SIMDVec 16 Int8
>constantSVec16 a = SVec16 a a a a
>                          a a a a
>                          a a a a
>                          a a a a

>constantCVec1 :: Complex Double -> SIMDVec 1 (Complex Double)
>constantCVec1 = CVec1
>
>constantCVec2 :: Complex Float -> SIMDVec 2 (Complex Float)
>constantCVec2 x = CVec2 x x

>instance Num (SIMDVec 16 Int8) where
>  (+) = zipSVec16 (+)
>  (-) = zipSVec16 (-)
>  (*) = zipSVec16 (*)
>  negate = mapSVec16 negate
>  abs = mapSVec16 abs
>  signum = mapSVec16 signum
>  fromInteger i = constantSVec16 (fromInteger i)


>instance Num (SIMDVec 8 Int16) where
>  (+) = zipSVec8 (+)
>  (-) = zipSVec8 (-)
>  (*) = zipSVec8 (*)
>  negate = mapSVec8 negate
>  abs = mapSVec8 abs
>  signum = mapSVec8 signum
>  fromInteger i = constantSVec8 (fromInteger i)

>instance Num (SIMDVec 4 Int32) where
>  (+) = zipSVec4 (+)
>  (-) = zipSVec4 (-)
>  (*) = zipSVec4 (*)
>  negate = mapSVec4 negate
>  abs = mapSVec4 abs
>  signum = mapSVec4 signum
>  fromInteger i = constantSVec4 (fromInteger i)

>instance Num (SIMDVec 2 Int64) where
>  (+) = zipSVec2 (+)
>  (-) = zipSVec2 (-)
>  (*) = zipSVec2 (*)
>  negate = mapSVec2 negate
>  abs = mapSVec2 abs
>  signum = mapSVec2 signum
>  fromInteger i = constantSVec2 (fromInteger i)

>instance Num (SIMDVec 4 Float) where
>  (+) = zipFVec4 (+)
>  (-) = zipFVec4 (-)
>  (*) = zipFVec4 (*)
>  negate = mapFVec4 negate
>  abs = mapFVec4 abs
>  signum = mapFVec4 signum
>  fromInteger i = constantFVec4 (fromInteger i)

>instance Fractional (SIMDVec 4 Float) where
>   (/) = zipFVec4 (/)
>   recip = mapFVec4 recip
>   fromRational r = constantFVec4 (fromRational r)

>instance Num (SIMDVec 2 Double) where
>  (+) = zipFVec2 (+)
>  (-) = zipFVec2 (-)
>  (*) = zipFVec2 (*)
>  negate = mapFVec2 negate
>  abs = mapFVec2 abs
>  signum = mapFVec2 signum
>  fromInteger i = constantFVec2 (fromInteger i)

>instance Num (SIMDVec 2 (Complex Float)) where
>  (+) = zipCVec2 (+)
>  (-) = zipCVec2 (-)
>  (*) = zipCVec2 (*)
>  negate = mapCVec2 negate
>  abs = mapCVec2 abs
>  signum = mapCVec2 signum
>  fromInteger i = CVec2 (fromInteger i :+ 0) (fromInteger i :+ 0)

>instance Num (SIMDVec 1 (Complex Double)) where
>  (+) = zipCVec1 (+)
>  (-) = zipCVec1 (-)
>  (*) = zipCVec1 (*)
>  negate = mapCVec1 negate
>  abs = mapCVec1 abs
>  signum = mapCVec1 signum
>  fromInteger i = CVec1 (fromInteger i :+ 0)

>instance Fractional (SIMDVec 2 Double) where
>   (/) = zipFVec2 (/)
>   recip = mapFVec2 recip
>   fromRational r = constantFVec2 (fromRational r)

>svec8_1 :: SIMDVec 8 Int16 -> Int16
>svec8_2 :: SIMDVec 8 Int16 -> Int16
>svec8_3 :: SIMDVec 8 Int16 -> Int16
>svec8_4 :: SIMDVec 8 Int16 -> Int16
>svec8_5 :: SIMDVec 8 Int16 -> Int16
>svec8_6 :: SIMDVec 8 Int16 -> Int16
>svec8_7 :: SIMDVec 8 Int16 -> Int16
>svec8_8 :: SIMDVec 8 Int16 -> Int16
>svec8_1 (SVec8 a _ _ _ _ _ _ _) = a
>svec8_2 (SVec8 _ b _ _ _ _ _ _) = b
>svec8_3 (SVec8 _ _ c _ _ _ _ _) = c
>svec8_4 (SVec8 _ _ _ d _ _ _ _) = d
>svec8_5 (SVec8 _ _ _ _ e _ _ _) = e
>svec8_6 (SVec8 _ _ _ _ _ f _ _) = f
>svec8_7 (SVec8 _ _ _ _ _ _ g _) = g
>svec8_8 (SVec8 _ _ _ _ _ _ _ h) = h

>makeFVec4 :: Float -> Float -> Float -> Float -> SIMDVec 4 Float
>makeFVec4 = FVec4

>makeFVec2 :: Double -> Double -> SIMDVec 2 Double
>makeFVec2 = FVec2

>makeSVec4 :: Int32 -> Int32 -> Int32 -> Int32 -> SIMDVec 4 Int32
>makeSVec4 = SVec4
> 
>makeSVec2 :: Int64 -> Int64 -> SIMDVec 2 Int64
>makeSVec2 = SVec2
>
>makeSVec8 :: Int16 -> Int16 -> Int16 -> Int16
>          -> Int16 -> Int16 -> Int16 -> Int16
>          -> SIMDVec 8 Int16
>makeSVec8 = SVec8

>makeSVec16 :: Int8 -> Int8 -> Int8 -> Int8
>           -> Int8 -> Int8 -> Int8 -> Int8
>           -> Int8 -> Int8 -> Int8 -> Int8
>           -> Int8 -> Int8 -> Int8 -> Int8
>           -> SIMDVec 16 Int8
>makeSVec16 = SVec16
>
>svec4_x :: SIMDVec 4 Int32 -> Int32
>svec4_y :: SIMDVec 4 Int32 -> Int32
>svec4_z :: SIMDVec 4 Int32 -> Int32
>svec4_t :: SIMDVec 4 Int32 -> Int32
>svec4_x (SVec4 a _ _ _) = a
>svec4_y (SVec4 _ b _ _) = b
>svec4_z (SVec4 _ _ c _) = c
>svec4_t (SVec4 _ _ _ d) = d

>svec2_x :: SIMDVec 2 Int64 -> Int64
>svec2_y :: SIMDVec 2 Int64 -> Int64
>svec2_x (SVec2 a _) = a
>svec2_y (SVec2 _ b) = b


>fvec4_x :: SIMDVec 4 Float -> Float
>fvec4_y :: SIMDVec 4 Float -> Float
>fvec4_z :: SIMDVec 4 Float -> Float
>fvec4_t :: SIMDVec 4 Float -> Float
>fvec4_x (FVec4 a _ _ _) = a
>fvec4_y (FVec4 _ b _ _) = b
>fvec4_z (FVec4 _ _ c _) = c
>fvec4_t (FVec4 _ _ _ d) = d

>instance VectorSpace (SIMDVec 2 Int64) where
>   type Scalar (SIMDVec 2 Int64) = Int64
>   vzero = SVec2 0 0 
>   vnegate = mapSVec2 negate
>   x %+ y = zipSVec2 (+) x y
>   x %* y = mapSVec2 (x *) y 
>instance VectorSpace (SIMDVec 4 Int32) where
>   type Scalar (SIMDVec 4 Int32) = Int32
>   vzero = SVec4 0 0 0 0 
>   vnegate = mapSVec4 negate
>   x %+ y = zipSVec4 (+) x y
>   x %* y = mapSVec4 (x *) y 
>instance VectorSpace (SIMDVec 8 Int16) where
>   type Scalar (SIMDVec 8 Int16) = Int16
>   vzero = SVec8 0 0 0 0 0 0 0 0
>   vnegate = mapSVec8 negate
>   x %+ y = zipSVec8 (+) x y
>   x %* y = mapSVec8 (x *) y 
>instance VectorSpace (SIMDVec 16 Int8) where
>   type Scalar (SIMDVec 16 Int8) = Int8
>   vzero = SVec16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
>   vnegate = mapSVec16 negate
>   x %+ y = zipSVec16 (+) x y
>   x %* y = mapSVec16 (x *) y
> 
>instance VectorSpace (SIMDVec 2 Double) where
>   type Scalar (SIMDVec 2 Double) = Double
>   vzero = FVec2 0 0
>   vnegate (FVec2 x y) = FVec2 (negate x) (negate y)
>   (FVec2 x y) %+ (FVec2 x' y') = FVec2 (x + x') (y + y')
>   k %* (FVec2 x y) = FVec2 (k * x) (k * y)

>instance VectorSpace (SIMDVec 4 Float) where
>   type Scalar (SIMDVec 4 Float) = Float
>   vzero = FVec4 0 0 0 0
>   vnegate (FVec4 a b c d) = FVec4 (negate a) (negate b) (negate c) (negate d)
>   (FVec4 a b c d) %+ (FVec4 a' b' c' d') = FVec4 (a + a') (b + b') (c + c') (d + d')
>   k %* (FVec4 a b c d) = FVec4 (k * a) (k * b) (k * c) (k * d)

>instance VectorSpace (SIMDVec 2 (Complex Float)) where
>   type Scalar (SIMDVec 2 (Complex Float)) = Complex Float
>   vzero = CVec2 0 0
>   vnegate (CVec2 x y) = CVec2 (negate x) (negate y)
>   (CVec2 x y) %+ (CVec2 x' y') = CVec2 (x + x') (y + y')
>   x %* (CVec2 a b) = CVec2 (x * a) (x * b)

>instance VectorSpace (SIMDVec 1 (Complex Double)) where
>   type Scalar (SIMDVec 1 (Complex Double)) = Complex Double
>   vzero = CVec1 0
>   vnegate (CVec1 x) = CVec1 (negate x)
>   (CVec1 x) %+ (CVec1 x') = CVec1 (x + x') 
>   x %* (CVec1 a) = CVec1 (x * a)

>fromInt8Vector16 :: SIMDVec 16 Int8 -> (Vector4 :*: Vector4) Int8
>fromInt8Vector16 v = Matrix $ Vector4
>   (Vector4 (svec16_1 v) (svec16_2 v) (svec16_3 v) (svec16_4 v))
>   (Vector4 (svec16_5 v) (svec16_6 v) (svec16_7 v) (svec16_8 v))
>   (Vector4 (svec16_9 v) (svec16_10 v) (svec16_11 v) (svec16_12 v))
>   (Vector4 (svec16_13 v) (svec16_13 v) (svec16_14 v) (svec16_15 v))

>toInt8Vector16 :: (Vector4 :*: Vector4) Int8 -> SIMDVec 16 Int8
>toInt8Vector16 (Matrix (Vector4 (Vector4 a1 b1 c1 d1)
>                    (Vector4 a2 b2 c2 d2)
>                    (Vector4 a3 b3 c3 d3)
>                    (Vector4 a4 b4 c4 d4)))
>     = makeSVec16 a1 b1 c1 d1 a2 b2 c2 d2 a3 b3 c3 d3 a4 b4 c4 d4

>fromDoubleVector2 :: Vector2 Double -> SIMDVec 2 Double
>fromDoubleVector2 (Vector2 a b) = FVec2 a b

>toDoubleVector2 :: SIMDVec 2 Double -> Vector2 Double
>toDoubleVector2 (FVec2 a b) = Vector2 a b

>fromFloatVector4 :: Vector4 Float -> SIMDVec 4 Float
>fromFloatVector4 (Vector4 a b c d) = FVec4 a b c d

>fromFloatVector3 :: Vector3 Float -> SIMDVec 4 Float
>fromFloatVector3 (Vector3 a b c) = FVec4 a b c 0.0
>
>toFloatVector4 :: SIMDVec 4 Float -> Vector4 Float
>toFloatVector4 (FVec4 a b c d) = Vector4 a b c d

>toFloatVector3 :: SIMDVec 4 Float -> Vector3 Float
>toFloatVector3 (FVec4 a b c _) = Vector3 a b c

>from2x2FloatVector4 :: (Vector2 :*: Vector2) Float -> SIMDVec 4 Float
>from2x2FloatVector4 (Matrix (Vector2 (Vector2 a b) (Vector2 a' b')))
> = FVec4 a b a' b'

>to2x2FloatVector4 :: SIMDVec 4 Float -> (Vector2 :*: Vector2) Float
>to2x2FloatVector4 (FVec4 a b a' b') = Matrix (Vector2 (Vector2 a b)
>                                                      (Vector2 a' b'))

>fromInt64Vector2 :: Vector2 Int64 -> SIMDVec 2 Int64
>fromInt64Vector2 (Vector2 a b) = makeSVec2 a b 

>fromInt32Vector4 :: Vector4 Int32 -> SIMDVec 4 Int32
>fromInt32Vector4 (Vector4 a b c d) = makeSVec4 a b c d

>fromInt16Vector8 :: Vector4 Int16 -> Vector4 Int16 -> SIMDVec 8 Int16
>fromInt16Vector8 (Vector4 a b c d) (Vector4 a' b' c' d') = makeSVec8 a b c d a' b' c' d'

>toInt16Vector8 :: SIMDVec 8 Int16 -> (Vector4 Int16, Vector4 Int16)
>toInt16Vector8 (SVec8 a b c d a' b' c' d') = (Vector4 a b c d, Vector4 a' b' c' d')

>toInt32Vector4 :: SIMDVec 4 Int32 -> Vector4 Int32
>toInt32Vector4 x = Vector4 (svec4_x x) (svec4_y x) (svec4_z x) (svec4_t x)

>toInt64Vector2 :: SIMDVec 2 Int64 -> Vector2 Int64
>toInt64Vector2 x = Vector2 (svec2_x x) (svec2_y x)

>fromComplexVector2 :: Vector2 (Complex Float) -> SIMDVec 2 (Complex Float)
>fromComplexVector2 (Vector2 a b) = CVec2 a b
>
>toComplexVector2 :: SIMDVec 2 (Complex Float) -> Vector2 (Complex Float) 
>toComplexVector2 (CVec2 a b) = Vector2 a b

>fromComplexVector1 :: Complex Double -> SIMDVec 1 (Complex Double)
>fromComplexVector1 = CVec1
>
>toComplexVector1 :: SIMDVec 1 (Complex Double) -> Complex Double
>toComplexVector1 (CVec1 x) = x

>transposeFloatVector4 :: SIMDVec 4 Float -> SIMDVec 4 Float
>transposeFloatVector4 (FVec4 a b c d) = FVec4 a c b d

>sum_coordinates2Int :: SIMDVec 2 Int64 -> Int64
>sum_coordinates2Int (SVec2 a b) = a + b
>sum_coordinates4Int :: SIMDVec 4 Int32 -> Int32
>sum_coordinates4Int (SVec4 a b c d) = a + b + c + d
>sum_coordinates8Int :: SIMDVec 8 Int16 -> Int16
>sum_coordinates8Int (SVec8 a b c d e f g h) = a + b + c + d + e + f + g + h
>sum_coordinates16Int :: SIMDVec 16 Int8 -> Int8
>sum_coordinates16Int (SVec16 a b c d e f g h a2 b2 c2 d2 e2 f2 g2 h2)
> = a + b + c + d + e + f + g + h + a2 + b2 + c2 + d2 + e2 + f2 + g2 + h2

>sum_coordinatesSIMD2 :: SIMDVec 2 Double -> Double
>sum_coordinatesSIMD2 (FVec2 a b) = a + b

>sum_coordinatesSIMD3 :: SIMDVec 4 Float -> Float
>sum_coordinatesSIMD3 (FVec4 a b c _) = a + b + c

>sum_coordinatesSIMD4 :: SIMDVec 4 Float -> Float
>sum_coordinatesSIMD4 (FVec4 a b c d) = a + b + c + d

>sum_coordinates2Complex :: SIMDVec 2 (Complex Float) -> Complex Float
>sum_coordinates2Complex (CVec2 a b) = a + b
>
>sum_coordinates1Complex :: SIMDVec 1 (Complex Double) -> Complex Double
>sum_coordinates1Complex (CVec1 x) = x

>sum_coordinates1Complex_components :: SIMDVec 1 (Complex Double) -> Double
>sum_coordinates1Complex_components (CVec1 (x :+ xi)) = x + xi

#endif


>instance Eq (SIMDVec 2 Int64) where
>   (==) = eqSVec2
>instance Eq (SIMDVec 4 Int32) where
>   (==) = eqSVec4
>instance Eq (SIMDVec 8 Int16) where
>   (==) = eqSVec8
>instance Eq (SIMDVec 16 Int8) where
>   (==) = eqSVec16

>instance Show (SIMDVec 2 Int64) where
>   show v = "(" ++ show (svec2_x v) ++ "," ++ show (svec2_y v) ++ ")"

>instance Show (SIMDVec 4 Int32) where
>   show v = "(" ++ show (svec4_x v) ++ "," ++ show (svec4_y v) ++ "," ++ show (svec4_z v) ++ "," ++ show (svec4_t v) ++ ")"

>instance Show (SIMDVec 8 Int16) where
>   show v = "(" ++
>            show (svec8_1 v) ++ "," ++ show (svec8_2 v) ++ ","
>            ++ show (svec8_3 v) ++ "," ++ show (svec8_4 v) ++ ","
>            ++ show (svec8_5 v) ++ "," ++ show (svec8_6 v) ++ ","
>            ++ show (svec8_7 v) ++ "," ++ show (svec8_8 v) ++ ")"
>            
>instance Show (SIMDVec 16 Int8) where
>   show v = "(" ++ show (svec16_1 v) ++ "," ++ show (svec16_2 v) ++ ","
>                ++ show (svec16_3 v) ++ "," ++ show (svec16_4 v) ++ ","
>                ++ show (svec16_5 v) ++ "," ++ show (svec16_6 v) ++ ","
>                ++ show (svec16_7 v) ++ "," ++ show (svec16_8 v) ++ ","
>                ++ show (svec16_9 v) ++ "," ++ show (svec16_10 v) ++ ","
>                ++ show (svec16_11 v) ++ "," ++ show (svec16_12 v) ++ ","
>                ++ show (svec16_13 v) ++ "," ++ show (svec16_14 v) ++ ","
>                ++ show (svec16_15 v) ++ "," ++ show (svec16_16 v) ++
>                ")"

>instance ConjugateSymmetric Int64 where { conj = id }
>instance ConjugateSymmetric Int32 where { conj = id }
>instance ConjugateSymmetric Int16 where { conj = id }
>instance ConjugateSymmetric Int8 where { conj = id }

>instance InnerProductSpace (SIMDVec 16 Int8) where
>   a %. b = sum_coordinates16Int (a * b)
>instance InnerProductSpace (SIMDVec 8 Int16) where
>   a %. b = sum_coordinates8Int (a * b)
>instance InnerProductSpace (SIMDVec 4 Int32) where
>   a %. b = sum_coordinates4Int (a * b)
>instance InnerProductSpace (SIMDVec 2 Int64) where
>   a %. b = sum_coordinates2Int (a * b)

>instance InnerProductSpace (SIMDVec 4 Float) where
>   a %. b = sum_coordinatesSIMD4 (a * b)

>instance (Scalar (SIMDVec 2 Double) ~ Double) => InnerProductSpace (SIMDVec 2 Double) where
>   a %. b = sum_coordinatesSIMD2 (a * b)

>instance InnerProductSpace (SIMDVec 2 (Complex Float)) where
>   a %. b = sum_coordinates2Complex (a * conjSIMD2 b)
>
>instance InnerProductSpace (SIMDVec 1 (Complex Double)) where
>   a %. b = sum_coordinates1Complex (a * conjSIMD1 b)

>instance NormedSpace (SIMDVec 2 (Complex Float)) where
>   norm a = sqrt (a %. a)
>   normSquared a = a %. a
>
>instance NormedSpace (SIMDVec 1 (Complex Double)) where
>   norm a = sqrt (a %. a)
>   normSquared a = a %. a

>class (VectorSpace a) => Optimal a where
>   type Optimized a
>   toO :: a -> Optimized a
>   fromO :: Optimized a -> a
>   zipO :: (Scalar a -> Scalar a -> Scalar a) -> Optimized a -> Optimized a -> Optimized a
>   mapO :: (Scalar a -> Scalar a) -> Optimized a -> Optimized a
>   sumCoordinatesO :: Optimized a -> Scalar a
>   constantO :: Scalar a -> Optimized a

>instance Optimal (Vector2 (Complex Float)) where
>   type Optimized (Vector2 (Complex Float)) = SIMDVec 2 (Complex Float)
>   toO = fromComplexVector2
>   fromO = toComplexVector2
>   zipO = zipCVec2
>   mapO = mapCVec2
>   sumCoordinatesO = sum_coordinates2Complex
>   constantO = constantCVec2

>instance Optimal (Complex Double) where
>   type Optimized (Complex Double) = SIMDVec 1 (Complex Double)
>   toO = fromComplexVector1
>   fromO = toComplexVector1
>   zipO = zipCVec1
>   mapO = mapCVec1
>   sumCoordinatesO = sum_coordinates1Complex
>   constantO = constantCVec1

>instance Optimal (Vector2 Double) where
>   type Optimized (Vector2 Double) = SIMDVec 2 Double
>   toO = fromDoubleVector2
>   fromO = toDoubleVector2
>   zipO = zipFVec2
>   mapO = mapFVec2
>   sumCoordinatesO = sum_coordinatesSIMD2
>   constantO = constantFVec2

>instance Optimal ((Vector4 :*: Vector2) Int16) where
>   type Optimized ((Vector4 :*: Vector2) Int16) = SIMDVec 8 Int16
>   toO (Matrix v) = fromInt16Vector8 (fmap xcoord2 v) (fmap ycoord2 v)
>   fromO x = let (a,b) = toInt16Vector8 x in Matrix (liftA2 Vector2 a b)
>   zipO = zipSVec8
>   mapO = mapSVec8
>   sumCoordinatesO = sum_coordinates8Int
>   constantO = constantSVec8

>instance Optimal ((Vector2 :*: Vector4) Int16) where
>   type Optimized ((Vector2 :*: Vector4) Int16) = SIMDVec 8 Int16
>   toO (Matrix (Vector2 x y)) = fromInt16Vector8 x y
>   fromO x = let (a,b) = toInt16Vector8 x in Matrix (Vector2 a b)
>   zipO = zipSVec8
>   mapO = mapSVec8
>   sumCoordinatesO = sum_coordinates8Int
>   constantO = constantSVec8

>instance Optimal (Vector4 Int16, Vector4 Int16) where
>   type Optimized (Vector4 Int16, Vector4 Int16) = SIMDVec 8 Int16
>   toO (a,b) = fromInt16Vector8 a b
>   fromO = toInt16Vector8
>   zipO = zipSVec8
>   mapO = mapSVec8
>   sumCoordinatesO = sum_coordinates8Int
>   constantO = constantSVec8

>instance Optimal (Vector2 Int64) where
>   type Optimized (Vector2 Int64) = SIMDVec 2 Int64
>   toO = fromInt64Vector2
>   fromO = toInt64Vector2
>   zipO = zipSVec2
>   mapO = mapSVec2
>   sumCoordinatesO = sum_coordinates2Int
>   constantO = constantSVec2

>instance Optimal (Vector4 Int32) where
>   type Optimized (Vector4 Int32) = SIMDVec 4 Int32
>   toO = fromInt32Vector4
>   fromO = toInt32Vector4
>   zipO = zipSVec4
>   mapO = mapSVec4
>   sumCoordinatesO = sum_coordinates4Int
>   constantO = constantSVec4

>instance Optimal ((Vector4 :*: Vector4) Int8) where
>   type Optimized ((Vector4 :*: Vector4) Int8) = SIMDVec 16 Int8
>   toO = toInt8Vector16
>   fromO = fromInt8Vector16
>   zipO = zipSVec16
>   mapO = mapSVec16
>   sumCoordinatesO = sum_coordinates16Int
>   constantO = constantSVec16

>instance Optimal ((Vector2 :*: Vector2) Int32) where
>   type Optimized ((Vector2 :*: Vector2) Int32) = SIMDVec 4 Int32
>   toO = from2x2Matrix
>   fromO = to2x2Matrix
>   zipO = zipSVec4
>   mapO = mapSVec4
>   sumCoordinatesO = sum_coordinates4Int
>   constantO = constantSVec4

>instance Optimal ((Vector2 :*: Vector2) Float) where
>   type Optimized ((Vector2 :*: Vector2) Float) = SIMDVec 4 Float
>   toO = from2x2FloatVector4
>   fromO = to2x2FloatVector4
>   zipO = zipFVec4
>   mapO = mapFVec4
>   sumCoordinatesO = sum_coordinatesSIMD4
>   constantO = constantFVec4

>instance Optimal (Vector4 Float) where
>   type Optimized (Vector4 Float) = SIMDVec 4 Float
>   toO = fromFloatVector4
>   fromO = toFloatVector4
>   zipO = zipFVec4
>   mapO = mapFVec4
>   sumCoordinatesO = sum_coordinatesSIMD4
>   constantO = constantFVec4

>instance Optimal (Vector3 Float) where
>   type Optimized (Vector3 Float) = SIMDVec 4 Float
>   toO = fromFloatVector3
>   fromO = toFloatVector3
>   zipO = zipFVec4
>   mapO = mapFVec4
>   sumCoordinatesO = sum_coordinatesSIMD3
>   constantO = constantFVec4

>{-# INLINABLE fast_multiply4_float #-}
>fast_multiply4_float :: Matrix4 Float -> Vector4 Float -> Vector4 Float
>fast_multiply4_float (Matrix (Vector4 a b c d)) v = Vector4
>   ((toO a) %. v') ((toO b) %. v') ((toO c) %. v') ((toO d) %. v')
>  where v' = toO v

>{-# INLINABLE fast_multiply3_float #-}
>fast_multiply3_float :: Matrix3 Float -> Vector3 Float -> Vector3 Float
>fast_multiply3_float (Matrix (Vector3 a b c)) v = Vector3
>   ((toO a) %. v') ((toO b) %. v') ((toO c) %. v')
>  where v' = toO v

>{-# INLINABLE fast_multiply2_double #-}
>fast_multiply2_double :: (Scalar (SIMDVec 2 Double) ~ Double) => Matrix2 Double -> Vector2 Double -> Vector2 Double
>fast_multiply2_double (Matrix (Vector2 a b)) v = Vector2
>   ((toO a) %. v') ((toO b) %. v')
>  where v' = toO v


>instance {-# OVERLAPS #-} (Scalar (SIMDVec 2 Double) ~ Double) => LinearTransform Vector2 Vector2 Double where
>   (<<*>) = fast_multiply2_double
>   v <*>> m = fast_multiply2_double (transposeImpl m) v

>instance {-# OVERLAPS #-} LinearTransform Vector3 Vector3 Float where
>   (<<*>) = fast_multiply3_float
>   v <*>> m = fast_multiply3_float (transposeImpl m) v

>instance {-# OVERLAPS #-} LinearTransform Vector4 Vector4 Float where
>   (<<*>) = fast_multiply4_float
>   v <*>> m = fast_multiply4_float (transposeImpl m) v

>instance {-# OVERLAPS #-} InnerProductSpace (Vector4 Float) where
>   a %. b = (toO a) %. (toO b)
>instance {-# OVERLAPS #-} InnerProductSpace (Vector3 Float) where
>   a %. b = (toO a) %. (toO b)
>instance {-# OVERLAPS #-} (Scalar (SIMDVec 2 Double) ~ Double) => InnerProductSpace (Vector2 Double) where
>   a %. b = (toO a) %. (toO b)

>instance {-# OVERLAPS #-} NormedSpace (Vector2 (Complex Float)) where
>   norm x = norm (toO x)
>   normSquared x = normSquared (toO x)

>instance {-# OVERLAPS #-} NormedSpace (Complex Double) where
>   norm x = norm (toO x)
>   normSquared x = normSquared (toO x)

>{-# SPECIALISE (%*%) :: (Vector4 :*: Vector4) Float -> (Vector4 :*: Vector4) Float -> (Vector4 :*: Vector4) Float #-}
>{-# SPECIALISE (%*%) :: (Vector3 :*: Vector3) Float -> (Vector3 :*: Vector3) Float -> (Vector3 :*: Vector3) Float #-}
>{-# SPECIALISE (%*%) :: (Vector2 :*: Vector2) Double -> (Vector2 :*: Vector2) Double -> (Vector2 :*: Vector2) Double #-}
