>{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TemplateHaskell #-}
>{-# LANGUAGE FlexibleInstances #-}
>module Math.Matrix.Test.InterfaceTest where
>import Data.Complex
>import Data.Ratio
>import qualified Data.Monoid as Monoid
>import Math.Test.Common
>import Test.QuickCheck
>import Test.HUnit
>import Math.Matrix.Interface

>commutative :: (Eq a, Show a, Arbitrary a) => (a -> a -> a) -> Property
>commutative f = counterexample "commutative" $ forAll arbitrary $ \ (r,r') -> f r r' == f r' r
>
>associative :: (Eq a, Show a, Arbitrary a) => (a -> a -> a) -> Property
>associative f = counterexample "associative" $ forAll arbitrary $ \ (r,r',r'') -> (r `f` r') `f` r'' === r `f` (r' `f` r'')

>identity_element :: (Eq a, Show a, Arbitrary a) => (a -> a -> a) -> a -> Property
>identity_element f e = counterexample "identity_element" $ forAll arbitrary $ \r -> (f r e === r) .&&. (f e r === r)

>inverse_element :: (Eq a, Show a, Arbitrary a) => (a -> a -> a) -> (a -> a) -> a -> Property
>inverse_element f neg e = counterexample "inverse_element" $ forAll arbitrary $ \r -> f r (neg r) == e .&&. f (neg r) r == e

>monoid :: (Eq a, Show a, Arbitrary a) => (a -> a -> a) -> a -> Property
>monoid f e = associative f .&&. identity_element f e

>group :: (Eq a, Show a, Arbitrary a) => (a -> a -> a) -> (a -> a) -> a -> Property
>group f neg e = monoid f e .&&. inverse_element f neg e

>commutative_group :: (Eq a, Show a, Arbitrary a) => (a -> a -> a) -> (a -> a) -> a -> Property
>commutative_group f neg e = commutative f .&&. group f neg e

>compatibility_of_scalar_multiplication (f :: v -> ()) = counterexample "compatibility of *" $
>     (forAll arbitrary $ \(a :: Scalar v, b :: Scalar v, c :: v)
>                         -> a %* (b %* c) === (a * b) %* c)

>multiplicative_identity (f :: v -> ()) = counterexample "identity of *" $
>     (forAll arbitrary $ \(a :: v) -> 1 %* a === a)

>distributivity_with_vector (f :: v -> ()) = counterexample "distributivity with vector +" $
>  forAll arbitrary $ \(a :: Scalar v, b :: v, c :: v) ->
>                      a %* (b %+ c) === ((a %* b) %+ (a %* c))

>distributivity_with_scalar (f :: v -> ()) = counterexample "distributivity with scalar +" $
>   forAll arbitrary $ \(a :: Scalar v, b :: Scalar v, c :: v) ->
>                       (a + b) %* c === ((a %* c) %+ (b %* c))

https://en.wikipedia.org/wiki/Vector_space

>vectorspace :: (Show v, Arbitrary v, Arbitrary (Scalar v), VectorSpace v, Eq v, Show (Scalar v)) => (v -> ()) -> Property
>vectorspace (f :: v -> ()) =
>   commutative_group ((%+) :: v -> v -> v) vnegate vzero
>    .&&. compatibility_of_scalar_multiplication f
>    .&&. multiplicative_identity f
>    .&&. distributivity_with_vector f
>    .&&. distributivity_with_scalar f

https://en.wikipedia.org/wiki/Inner_product_space

>conjugate_symmetry (f :: v -> ()) =
>   counterexample "conjugate_symmetry" $
>     forAll arbitrary $ \ (x :: v, y :: v) -> ((x %. y) === conj (y %. x))

>linearity_first_argument (f :: v -> ()) =
>     counterexample "linearity_first_argument" $
>     ( (forAll arbitrary $ \ (a :: Scalar v, x :: v, y :: v) ->
>        (a %* x) %. y === a * (x %. y))
> .&&. (forAll arbitrary $ \ (x :: v, y :: v, z :: v) ->
>        (x %+ y) %. z === (x %. z) %+ (y %. z)))

>positive_definiteness :: (Num (Scalar v), Show (Scalar v), Ord (Scalar v), Show v, InnerProductSpace v, VectorSpace v,Arbitrary v) => (v -> ()) -> Property
>positive_definiteness (f :: v -> ()) = counterexample "positive_definiteness" $
>      ( (forAll arbitrary $ \ (x :: v) -> (x %. x) >= 0)
>  .&&. (((vzero :: v) %. vzero) === 0))
>
>        -- (forAll arbitrary $ \ (x :: v) -> ((x %. x) == 0) ==> (x === vzero)))

>inner_product_space (f :: v -> ()) = counterexample "innerproductspace" $ 
>   vectorspace f .&&. conjugate_symmetry f
>     .&&. linearity_first_argument f .&&. positive_definiteness f


>-- | <https://en.wikipedia.org/wiki/Lie_algebra>

>bilinearity (f :: v -> ()) = counterexample "bilinearity" $
>   forAll arbitrary $ \ (a :: Scalar v, b :: Scalar v, x :: v, y :: v, z :: v) ->
>      (a %* x %+ b %* y) %<>% z === a %* (x %<>% z) %+ b %* (y %<>% z)

>alternativity (f :: v -> ()) = counterexample "alternativity" $
>   forAll arbitrary $ \ (a :: v) -> a %<>% a === vzero

>jacobi_identity (f :: v -> ()) = counterexample "jacobi_identity" $
>   forAll arbitrary $ \ (x :: v, y :: v, z :: v) ->
>      x %<>% (y %<>% z) %+
>      z %<>% (x %<>% y) %+
>      y %<>% (z %<>% x) === vzero

>anticommutativity (f :: v -> ()) = counterexample "anticommutativity" $
>   forAll arbitrary $ \ (x :: v, y :: v) -> x %<>% y === vnegate (y %<>% x)

>lie_algebra (f :: v -> ()) = counterexample "lie_algebra" $
>   bilinearity f .&&. alternativity f .&&. jacobi_identity f .&&. anticommutativity f


https://en.wikipedia.org/wiki/Vector_space

>testVectorSpace :: (VectorSpace w, Num w,Show w, Eq w, Arbitrary w,
>                    Show (Scalar w), Arbitrary (Scalar w)) => (w -> ()) -> Test
>testVectorSpace (f :: (w -> ())) = "VectorSpace instance" ~: test [
>   vectorspace f ~? "vector space"
>  ]

>prop_rational_ips = inner_product_space (const () :: Rational -> ())
>prop_integer_ips = inner_product_space (const () :: Integer -> ())
>prop_int_ips = inner_product_space (const () :: Int -> ())
>prop_pair_ips = inner_product_space (const () :: (Integer,Integer) -> ())
>prop_triple_ips = inner_product_space (const () :: (Integer,Integer,Integer) -> ())


>instance Eq (Monoid.Endo Rational) where
>  (Monoid.Endo f) == (Monoid.Endo g) = and [f x == g x | x <- [0,1%2..5]]

>instance Show (Monoid.Endo Rational) where
>  show (Monoid.Endo f) = "[" ++ concatMap (\i -> show i ++ "=" ++ show (f i) ++ ";") [0,1%2..5] ++ "]"

> -- prop_float_vectorspace = vectorspace (const () :: Float -> ())
> -- prop_double_vectorspace = vectorspace (const () :: Double -> ())
> -- prop_complex_float_vectorspace = vectorspace (const () :: Complex Float -> ())
> -- prop_complex_double_vectorspace = vectorspace (const () :: Complex Double -> ())

>$(return [])
>qcAll = $quickCheckAll

>tests = test [
>   qcAll
>   ]
