>{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
>module Math.Matrix.Test.SIMDTests where
>import Test.HUnit
>import Test.QuickCheck
>import GHC.Int
>import Math.Matrix.SIMD
>import qualified Math.Matrix.Test.InterfaceTest as MatrixTest

>instance Arbitrary (SVec16 Int8) where
>   arbitrary = do
>       x1 <- arbitrary
>       x2 <- arbitrary
>       x3 <- arbitrary
>       x4 <- arbitrary
>       x5 <- arbitrary
>       x6 <- arbitrary
>       x7 <- arbitrary
>       x8 <- arbitrary
>       x9 <- arbitrary
>       x10 <- arbitrary
>       x11 <- arbitrary
>       x12 <- arbitrary
>       x13 <- arbitrary
>       x14 <- arbitrary
>       x15 <- arbitrary
>       x16 <- arbitrary
>       return $ makeSVec16 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16                  
>instance Arbitrary (SVec8 Int16) where
>   arbitrary = do
>       x1 <- arbitrary
>       x2 <- arbitrary
>       x3 <- arbitrary
>       x4 <- arbitrary
>       x5 <- arbitrary
>       x6 <- arbitrary
>       x7 <- arbitrary
>       x8 <- arbitrary
>       return $ makeSVec8 x1 x2 x3 x4 x5 x6 x7 x8
>instance Arbitrary (SVec4 Int32) where
>   arbitrary = do
>       x1 <- arbitrary
>       x2 <- arbitrary
>       x3 <- arbitrary
>       x4 <- arbitrary
>       return $ makeSVec4 x1 x2 x3 x4

>prop_vectorSpace_svec16 = MatrixTest.vectorspace (const () :: SVec16 Int8 -> ())
>prop_vectorSpace_svec8  = MatrixTest.vectorspace (const () :: SVec8 Int16 -> ())
>prop_vectorSpace_svec4  = MatrixTest.vectorspace (const () :: SVec4 Int32 -> ())


>$(return [])
>qcAll = $(quickCheckAll)

>tests = "SIMD" ~: test [
>   "properties" ~: qcAll
>   ]
