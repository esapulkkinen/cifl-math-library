>{-# LANGUAGE TypeOperators, TemplateHaskell, FlexibleContexts, ScopedTypeVariables #-}
>module Math.Matrix.Test.Vector3Test where
>import Test.HUnit
>import Test.QuickCheck
>import Math.Matrix.Matrix
>import Math.Matrix.Interface
>import Math.Matrix.Vector1
>import Math.Matrix.Vector3
>import Math.Matrix.Vector2
>import qualified Math.Matrix.Test.InterfaceTest as MatrixTest

>instance (Arbitrary (f (g a))) => Arbitrary ((:*:) f g a) where
>   arbitrary = do x <- arbitrary
>                  return $ Matrix x

>instance (Arbitrary a) => Arbitrary (Vector3 a) where
>   arbitrary = do (x,y,z) <- arbitrary
>                  return $ Vector3 x y z
>instance (Arbitrary a) => Arbitrary (Vector2 a) where
>   arbitrary = do (x,y) <- arbitrary
>                  return $ Vector2 x y
>
>instance (Arbitrary a) => Arbitrary (Vector1 a) where
>   arbitrary = do x <- arbitrary
>                  return $ Vector1 x 


https://en.wikipedia.org/wiki/Eigenvalues_and_eigenvectors

>checkEigenValue :: (Vector3 :*: Vector3) Float -> Vector3 Float -> Float -> Test
>checkEigenValue a v l = (a <<*> v) ~=? (1 %* v)

>checkEigenValueTrace :: (Vector3 :*: Vector3) Float -> Test
>checkEigenValueTrace a = trace a ~=? sum_coordinates (eigenvalues a)

>testEigenValue3_identity = checkEigenValue identity (Vector3 1 2 3) 1
>testEigenValue3_trace_identity    = checkEigenValueTrace identity

>prop_vector3_vectorspace = MatrixTest.vectorspace (const () :: Vector3 Integer -> ())
>prop_vector2_vectorspace = MatrixTest.vectorspace (const () :: Vector2 Integer -> ())
>prop_vector1_vectorspace = MatrixTest.vectorspace (const () :: Vector1 Integer -> ())
>prop_vector3_liealgebra  = MatrixTest.lie_algebra (const () :: Vector3 Integer -> ())
>prop_vector3_rational_liealgebra = MatrixTest.lie_algebra (const () :: Vector3 Rational -> ())
> -- prop_vector2_liealgebra  = MatrixTest.lie_algebra (const () :: Vector2 Integer -> ())
> -- prop_vector1_liealgebra  = MatrixTest.lie_algebra (const () :: Vector1 Integer -> ())
>prop_vector1_vector3_vs = MatrixTest.vectorspace (const () :: (Vector1 :*: Vector3) Integer -> ())
>prop_vector3_vector1_vs = MatrixTest.vectorspace (const () :: (Vector3 :*: Vector1) Integer -> ())
>prop_vector2_vector3_vs = MatrixTest.vectorspace (const () :: (Vector2 :*: Vector3) Integer -> ())
>prop_vector3_vector2_vs = MatrixTest.vectorspace (const () :: (Vector3 :*: Vector2) Integer -> ())
>prop_vector3_vector3_vs = MatrixTest.vectorspace (const () :: (Vector3 :*: Vector3) Integer -> ())


>$(return [])
>qcAll = $(quickCheckAll)

>tests = "Vector3" ~: test [
>   "properties" ~: qcAll,
>   "testEigenValue3_identity" ~: testEigenValue3_identity,
>   "testEigenValue3_trace_identity" ~: testEigenValue3_trace_identity
>   ]
