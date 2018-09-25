>{-# LANGUAGE CPP #-}
>{-# LANGUAGE GADTs, FlexibleInstances, TypeFamilies, DeriveFunctor #-}
>{-# LANGUAGE ScopedTypeVariables #-}
>{-# LANGUAGE MagicHash, UnboxedTuples #-}
>-- | This modules provides SIMD optimized versions of vectors.
>-- Relies heavily on GHC SIMD support. SIMD works only when using LLVM and
>-- GHC 8.x
>-- The only combinations of vectors supported are 4 * Int32, 8 * Int16 and 16 * Int8.
>module Math.Matrix.SIMD where
>
>import Control.Applicative
>import Math.Matrix.Interface

#if __GLASGOW_HASKELL__ >= 800 && __GLASGOW_HASKELL_LLVM__ >= 500

#include "MachDeps.h"

>{-# OPTIONS_GHC -fllvm #-}

>import GHC.Exts
>import GHC.Int
>import GHC.Word

>data SVec4 a where
>   Int32SVec4 :: Int32X4# -> SVec4 Int32

>eqSVec4 :: SVec4 Int32 -> SVec4 Int32 -> Bool
>eqSVec4 (Int32SVec4 x) (Int32SVec4 y) = case unpackInt32X4# (minusInt32X4# x y) of
>    (# a,b,c,d #) -> (I32# a) == 0 && (I32# b) == 0 && (I32# c) == 0 && (I32# d) == 0
>eqSVec8 :: SVec8 Int16 -> SVec8 Int16 -> Bool
>eqSVec8 (Int16SVec8 x) (Int16SVec8 y) = case unpackInt16X8# (minusInt16X8# x y) of
>    (# a,b,c,d,e,f,g,h #) -> (I16# a) == 0 && (I16# b) == 0 && (I16# c) == 0 && (I16# d) == 0 &&
>                             (I16# e) == 0 && (I16# f) == 0 && (I16# g) == 0 && (I16# h) == 0
>eqSVec16 :: SVec16 Int8 -> SVec16 Int8 -> Bool
>eqSVec16 (Int8SVec16 x) (Int8SVec16 y) = case unpackInt8X16# (minusInt8X16# x y) of
>     (# a,b,c,d,e,f,g,h, aa,bb,cc,dd,ee,ff,gg,hh #) ->
>         (I8# a) == 0 && (I8# b) == 0 && (I8# c) == 0 && (I8# d) == 0 &&
>         (I8# e) == 0 && (I8# f) == 0 && (I8# g) == 0 && (I8# h) == 0 &&
>         (I8# aa) == 0 && (I8# bb) == 0 && (I8# cc) == 0 && (I8# dd) == 0 &&
>         (I8# ee) == 0 && (I8# ff) == 0 && (I8# gg) == 0 && (I8# hh) == 0

>zipSVec4 :: (Int32 -> Int32 -> Int32) -> SVec4 Int32 -> SVec4 Int32 -> SVec4 Int32
>zipSVec4 f (Int32SVec4 x) (Int32SVec4 y) = let
>   (# a,b,c,d #) = unpackInt32X4# x
>   (# a',b',c',d' #) = unpackInt32X4# y
>   I32# ra = f (I32# a) (I32# a')
>   I32# rb = f (I32# b) (I32# b')
>   I32# rc = f (I32# c) (I32# c')
>   I32# rd = f (I32# d) (I32# d')
> in Int32SVec4 (packInt32X4# (# ra,rb,rc,rd #))

>zipSVec8 :: (Int16 -> Int16 -> Int16) -> SVec8 Int16 -> SVec8 Int16 -> SVec8 Int16
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

>zipSVec16 :: (Int8 -> Int8 -> Int8) -> SVec16 Int8 -> SVec16 Int8 -> SVec16 Int8
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
>mapSVec4 :: (Int32 -> Int32) -> SVec4 Int32 -> SVec4 Int32
>mapSVec4 f (Int32SVec4 x) = let (# a,b,c,d #) = unpackInt32X4# x
>                                I32# fa = f (I32# a)
>                                I32# fb = f (I32# b)
>                                I32# fc = f (I32# c)
>                                I32# fd = f (I32# d)
>                             in Int32SVec4 (packInt32X4# (# fa,fb,fc,fd #))

>svec4_x :: SVec4 Int32 -> Int32
>svec4_x (Int32SVec4 x# ) = let (# a,b,c,d #) = unpackInt32X4# x# in I32# a
>
>svec4_y :: SVec4 Int32 -> Int32
>svec4_y (Int32SVec4 x# ) = let (# a,b,c,d #) = unpackInt32X4# x# in I32# b

>svec4_z ::SVec4 Int32 -> Int32
>svec4_z (Int32SVec4 x# ) = let (# a,b,c,d #) = unpackInt32X4# x# in I32# c

>svec4_t :: SVec4 Int32 -> Int32
>svec4_t (Int32SVec4 x# ) = let (# a,b,c,d #) = unpackInt32X4# x# in I32# d

>data SVec8 a where
>   Int16SVec8 :: Int16X8# -> SVec8 Int16

>mapSVec8 :: (Int16 -> Int16) -> SVec8 Int16 -> SVec8 Int16
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


>svec8_1 :: SVec8 Int16 -> Int16
>svec8_2 :: SVec8 Int16 -> Int16
>svec8_3 :: SVec8 Int16 -> Int16
>svec8_4 :: SVec8 Int16 -> Int16
>svec8_5 :: SVec8 Int16 -> Int16
>svec8_6 :: SVec8 Int16 -> Int16
>svec8_7 :: SVec8 Int16 -> Int16
>svec8_8 :: SVec8 Int16 -> Int16
>svec8_1 (Int16SVec8 x# ) = let (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x# in I16# a
>svec8_2 (Int16SVec8 x# ) = let (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x# in I16# b
>svec8_3 (Int16SVec8 x# ) = let (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x# in I16# c
>svec8_4 (Int16SVec8 x# ) = let (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x# in I16# d
>svec8_5 (Int16SVec8 x# ) = let (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x# in I16# e
>svec8_6 (Int16SVec8 x# ) = let (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x# in I16# f
>svec8_7 (Int16SVec8 x# ) = let (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x# in I16# g
>svec8_8 (Int16SVec8 x# ) = let (# a,b,c,d,e,f,g,h #) = unpackInt16X8# x# in I16# h


>svec16_1 :: SVec16 Int8 -> Int8
>svec16_2 :: SVec16 Int8 -> Int8
>svec16_3 :: SVec16 Int8 -> Int8
>svec16_4 :: SVec16 Int8 -> Int8
>svec16_5 :: SVec16 Int8 -> Int8
>svec16_6 :: SVec16 Int8 -> Int8
>svec16_7 :: SVec16 Int8 -> Int8
>svec16_8 :: SVec16 Int8 -> Int8
>svec16_9 :: SVec16 Int8 -> Int8
>svec16_10 :: SVec16 Int8 -> Int8
>svec16_11 :: SVec16 Int8 -> Int8
>svec16_12 :: SVec16 Int8 -> Int8
>svec16_13 :: SVec16 Int8 -> Int8
>svec16_14 :: SVec16 Int8 -> Int8
>svec16_15 :: SVec16 Int8 -> Int8
>svec16_16 :: SVec16 Int8 -> Int8
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

>data SVec16 a where
>   Int8SVec16  :: Int8X16# -> SVec16 Int8

>mapSVec16 :: (Int8  -> Int8) -> SVec16 Int8 -> SVec16 Int8
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


>constantSVec4 :: Int32 -> SVec4 Int32
>constantSVec4 (I32# x# ) = Int32SVec4 (broadcastInt32X4# x# )

>constantSVec8 :: Int16 -> SVec8 Int16
>constantSVec8 (I16# x# ) = Int16SVec8 (broadcastInt16X8# x# )
>
>constantSVec16 :: Int8 -> SVec16 Int8
>constantSVec16 (I8# x# ) = Int8SVec16 (broadcastInt8X16# x# )

>makeSVec4 :: Int32 -> Int32 -> Int32 -> Int32 -> SVec4 Int32
>makeSVec4 (I32# a) (I32# b) (I32# c) (I32# d)
>   = Int32SVec4 (packInt32X4# (# a, b, c, d #) )

>makeSVec8 :: Int16 -> Int16 -> Int16 -> Int16
>          -> Int16 -> Int16 -> Int16 -> Int16
>          -> SVec8 Int16
>makeSVec8 (I16# a) (I16# b) (I16# c) (I16# d) (I16# e) (I16# f) (I16# g) (I16# h) = Int16SVec8 (packInt16X8# (# a,b,c,d,e,f,g,h #))
>
>makeSVec16 :: Int8 -> Int8 -> Int8 -> Int8
>           -> Int8 -> Int8 -> Int8 -> Int8
>           -> Int8 -> Int8 -> Int8 -> Int8
>           -> Int8 -> Int8 -> Int8 -> Int8
>           -> SVec16 Int8
>makeSVec16 (I8# a) (I8# b) (I8# c) (I8# d)
>           (I8# e) (I8# f) (I8# g) (I8# h)
>           (I8# a') (I8# b') (I8# c') (I8# d')
>           (I8# e') (I8# f') (I8# g') (I8# h')
>   = Int8SVec16 (packInt8X16# (# a,b,c,d,e,f,g,h,a',b',c',d',e',f',g',h' #))

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

>instance VectorSpace (SVec16 Int8) where
>   type Scalar (SVec16 Int8) = Int8
>   vzero = Int8SVec16 (broadcastInt8X16# 0#)
>   vnegate (Int8SVec16 i) = Int8SVec16 (negateInt8X16# i)
>   (Int8SVec16 x) %+ (Int8SVec16 y) = Int8SVec16 (plusInt8X16# x y)
>   (I8# x) %* (Int8SVec16 v) = Int8SVec16 (timesInt8X16# (broadcastInt8X16# x) v)

#else

#warning "SIMD optimizations are disabled. -fllvm is required for SIMD optimizations."

>import GHC.Int

>data SVec4 a where
>   SVec4 :: Int32 -> Int32 -> Int32 -> Int32 -> SVec4 Int32
>data SVec8 a where
>   SVec8 :: Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> SVec8 Int16
>data SVec16 a where
>   SVec16 :: Int8 -> Int8 -> Int8 -> Int8
>          -> Int8 -> Int8 -> Int8 -> Int8
>          -> Int8 -> Int8 -> Int8 -> Int8
>          -> Int8 -> Int8 -> Int8 -> Int8
>          -> SVec16 Int8

>eqSVec4 :: SVec4 Int32 -> SVec4 Int32 -> Bool
>eqSVec4 (SVec4 a b c d) (SVec4 a' b' c' d') = a == a' && b == b' && c == c' && d == d'
>eqSVec8 :: SVec8 Int16 -> SVec8 Int16 -> Bool
>eqSVec8 (SVec8 a b c d e f g h) (SVec8 a' b' c' d' e' f' g' h') =
>    a == a' && b == b' && c == c' && d == d' &&
>    e == e' && f == f' && g == g' && h == h'
>eqSVec16 :: SVec16 Int8 -> SVec16 Int8 -> Bool
>eqSVec16 (SVec16 a b c d e f g h aa bb cc dd ee ff gg hh)
>         (SVec16 a' b' c' d' e' f' g' h' aa' bb' cc' dd' ee' ff' gg' hh') =
>              a == a' && b == b' && c == c' && d == d' &&
>              e == e' && f == f' && g == g' && h == h' &&
>              aa == aa' && bb == bb' && cc == cc' && dd == dd' &&
>              ee == ee' && ff == ff' && gg == gg' && hh == hh'

>svec16_1 :: SVec16 Int8 -> Int8
>svec16_2 :: SVec16 Int8 -> Int8
>svec16_3 :: SVec16 Int8 -> Int8
>svec16_4 :: SVec16 Int8 -> Int8
>svec16_5 :: SVec16 Int8 -> Int8
>svec16_6 :: SVec16 Int8 -> Int8
>svec16_7 :: SVec16 Int8 -> Int8
>svec16_8 :: SVec16 Int8 -> Int8
>svec16_9 :: SVec16 Int8 -> Int8
>svec16_10 :: SVec16 Int8 -> Int8
>svec16_11 :: SVec16 Int8 -> Int8
>svec16_12 :: SVec16 Int8 -> Int8
>svec16_13 :: SVec16 Int8 -> Int8
>svec16_14 :: SVec16 Int8 -> Int8
>svec16_15 :: SVec16 Int8 -> Int8
>svec16_16 :: SVec16 Int8 -> Int8
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


>mapSVec16 :: (Int8  -> Int8) -> SVec16 Int8 -> SVec16 Int8
>mapSVec16 fu (SVec16 a b c d e f g h a' b' c' d' e' f' g' h')
>  = SVec16 (fu a) (fu b) (fu c) (fu d) (fu e) (fu f) (fu g) (fu h)
>          (fu a') (fu b') (fu c') (fu d') (fu e') (fu f') (fu g') (fu h')
>mapSVec8 :: (Int16 -> Int16) -> SVec8 Int16 -> SVec8 Int16
>mapSVec8 fu (SVec8 a b c d e f g h)
>   = SVec8 (fu a) (fu b) (fu c) (fu d) (fu e) (fu f) (fu g) (fu h)

>mapSVec4 :: (Int32 -> Int32) -> SVec4 Int32 -> SVec4 Int32
>mapSVec4 fu (SVec4 a b c d) = SVec4 (fu a) (fu b) (fu c) (fu d)

>zipSVec4 :: (Int32 -> Int32 -> Int32) -> SVec4 Int32 -> SVec4 Int32 -> SVec4 Int32
>zipSVec4 fu (SVec4 a b c d) (SVec4 a' b' c' d')
>  = SVec4 (fu a a') (fu b b') (fu c c') (fu d d')

>zipSVec8 :: (Int16 -> Int16 -> Int16) -> SVec8 Int16 -> SVec8 Int16 -> SVec8 Int16
>zipSVec8 fu (SVec8 a b c d e f g h) (SVec8 a' b' c' d' e' f' g' h')
>  = SVec8 (fu a a') (fu b b') (fu c c') (fu d d')
>          (fu e e') (fu f f') (fu g g') (fu h h')

>zipSVec16 :: (Int8 -> Int8 -> Int8) -> SVec16 Int8 -> SVec16 Int8 -> SVec16 Int8
>zipSVec16 fu (SVec16 a b c d e f g h aa bb cc dd ee ff gg hh)
>             (SVec16 a' b' c' d' e' f' g' h' aa' bb' cc' dd' ee' ff' gg' hh')
>  = SVec16 (fu a a') (fu b b') (fu c c') (fu d d')
>          (fu e e') (fu f f') (fu g g') (fu h h')
>          (fu aa aa') (fu bb bb') (fu cc cc') (fu dd dd')
>          (fu ee ee') (fu ff ff') (fu gg gg') (fu hh hh')


>constantSVec4 :: Int32 -> SVec4 Int32
>constantSVec4 a = SVec4 a a a a

>constantSVec8 :: Int16 -> SVec8 Int16
>constantSVec8 a = SVec8 a a a a a a a a

>constantSVec16 :: Int8 -> SVec16 Int8
>constantSVec16 a = SVec16 a a a a
>                          a a a a
>                          a a a a
>                          a a a a

>svec8_1 :: SVec8 Int16 -> Int16
>svec8_2 :: SVec8 Int16 -> Int16
>svec8_3 :: SVec8 Int16 -> Int16
>svec8_4 :: SVec8 Int16 -> Int16
>svec8_5 :: SVec8 Int16 -> Int16
>svec8_6 :: SVec8 Int16 -> Int16
>svec8_7 :: SVec8 Int16 -> Int16
>svec8_8 :: SVec8 Int16 -> Int16
>svec8_1 (SVec8 a _ _ _ _ _ _ _) = a
>svec8_2 (SVec8 _ b _ _ _ _ _ _) = b
>svec8_3 (SVec8 _ _ c _ _ _ _ _) = c
>svec8_4 (SVec8 _ _ _ d _ _ _ _) = d
>svec8_5 (SVec8 _ _ _ _ e _ _ _) = e
>svec8_6 (SVec8 _ _ _ _ _ f _ _) = f
>svec8_7 (SVec8 _ _ _ _ _ _ g _) = g
>svec8_8 (SVec8 _ _ _ _ _ _ _ h) = h

>makeSVec4 :: Int32 -> Int32 -> Int32 -> Int32 -> SVec4 Int32
>makeSVec4 = SVec4
>
>makeSVec8 :: Int16 -> Int16 -> Int16 -> Int16
>          -> Int16 -> Int16 -> Int16 -> Int16
>          -> SVec8 Int16
>makeSVec8 = SVec8

>makeSVec16 :: Int8 -> Int8 -> Int8 -> Int8
>           -> Int8 -> Int8 -> Int8 -> Int8
>           -> Int8 -> Int8 -> Int8 -> Int8
>           -> Int8 -> Int8 -> Int8 -> Int8
>           -> SVec16 Int8
>makeSVec16 = SVec16
>
>svec4_x :: SVec4 Int32 -> Int32
>svec4_y :: SVec4 Int32 -> Int32
>svec4_z :: SVec4 Int32 -> Int32
>svec4_t :: SVec4 Int32 -> Int32
>svec4_x (SVec4 a _ _ _) = a
>svec4_y (SVec4 _ b _ _) = b
>svec4_z (SVec4 _ _ c _) = c
>svec4_t (SVec4 _ _ _ d) = d



>instance VectorSpace (SVec4 Int32) where
>   type Scalar (SVec4 Int32) = Int32
>   vzero = SVec4 0 0 0 0 
>   vnegate = mapSVec4 negate
>   x %+ y = zipSVec4 (+) x y
>   x %* y = mapSVec4 (x *) y 
>instance VectorSpace (SVec8 Int16) where
>   type Scalar (SVec8 Int16) = Int16
>   vzero = SVec8 0 0 0 0 0 0 0 0
>   vnegate = mapSVec8 negate
>   x %+ y = zipSVec8 (+) x y
>   x %* y = mapSVec8 (x *) y 
>instance VectorSpace (SVec16 Int8) where
>   type Scalar (SVec16 Int8) = Int8
>   vzero = SVec16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
>   vnegate = mapSVec16 negate
>   x %+ y = zipSVec16 (+) x y
>   x %* y = mapSVec16 (x *) y 

#endif

>instance Eq (SVec4 Int32) where
>   (==) = eqSVec4
>instance Eq (SVec8 Int16) where
>   (==) = eqSVec8
>instance Eq (SVec16 Int8) where
>   (==) = eqSVec16

>instance Show (SVec4 Int32) where
>   show v = "(" ++ show (svec4_x v) ++ "," ++ show (svec4_y v) ++ "," ++ show (svec4_z v) ++ "," ++ show (svec4_t v) ++ ")"

>instance Show (SVec8 Int16) where
>   show v = "(" ++
>            show (svec8_1 v) ++ "," ++ show (svec8_2 v) ++ ","
>            ++ show (svec8_3 v) ++ "," ++ show (svec8_4 v) ++ ","
>            ++ show (svec8_5 v) ++ "," ++ show (svec8_6 v) ++ ","
>            ++ show (svec8_7 v) ++ "," ++ show (svec8_8 v) ++ ")"
>            
>instance Show (SVec16 Int8) where
>   show v = "(" ++ show (svec16_1 v) ++ "," ++ show (svec16_2 v) ++ ","
>                ++ show (svec16_3 v) ++ "," ++ show (svec16_4 v) ++ ","
>                ++ show (svec16_5 v) ++ "," ++ show (svec16_6 v) ++ ","
>                ++ show (svec16_7 v) ++ "," ++ show (svec16_8 v) ++ ","
>                ++ show (svec16_9 v) ++ "," ++ show (svec16_10 v) ++ ","
>                ++ show (svec16_11 v) ++ "," ++ show (svec16_12 v) ++ ","
>                ++ show (svec16_13 v) ++ "," ++ show (svec16_14 v) ++ ","
>                ++ show (svec16_15 v) ++ "," ++ show (svec16_16 v) ++
>                ")"
