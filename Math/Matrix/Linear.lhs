>-- -*- coding: utf-8 -*-
>{-# LANGUAGE Safe, GADTs, RankNTypes, TypeOperators, ScopedTypeVariables #-}
>{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
>{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
>{-# LANGUAGE UndecidableInstances, Arrows #-}
>{-# LANGUAGE DeriveGeneric, StandaloneDeriving, InstanceSigs #-}
>module Math.Matrix.Linear where
>import safe Prelude hiding (id,(.))
>import safe Control.Category
>import safe Control.Arrow
>import safe Control.Applicative
>import safe Data.Typeable
>import safe GHC.Generics hiding ((:*:))
>import safe Math.Matrix.Interface
>import safe Math.Number.Interface
>import safe Math.Number.StreamInterface
>import safe Math.Matrix.Vector1
>import safe Math.Matrix.Vector2
>import safe Math.Matrix.Vector3
>import safe Math.Matrix.Vector4
>import safe Math.Matrix.FiniteVector
>import safe Math.Matrix.Points
>import safe Math.Tools.Isomorphism
>import safe Math.Tools.NaturalTransformation
>import safe Math.Tools.Prop
>import safe Math.Tools.I
>import safe Math.Tools.Arrow
>import safe Math.Tools.Universe
>import safe Math.Tools.PrettyP

>-- | Linear maps <https://en.wikipedia.org/wiki/Linear_map>
>data LinearMap v w where
>   MatIdentity :: LinearMap b b
>   Mat11 :: (Num a, ConjugateSymmetric a) => (Vector1 :*: Vector1) a -> LinearMap (Vector1 a) (Vector1 a)
>   Mat12 :: (Num a, ConjugateSymmetric a) => (Vector1 :*: Vector2) a -> LinearMap (Vector1 a) (Vector2 a)
>   Mat13 :: (Num a, ConjugateSymmetric a) => (Vector1 :*: Vector3) a -> LinearMap (Vector1 a) (Vector3 a)
>   Mat14 :: (Num a, ConjugateSymmetric a) => (Vector1 :*: Vector4) a -> LinearMap (Vector1 a) (Vector4 a)
>   Mat21 :: (Num a, ConjugateSymmetric a) => (Vector2 :*: Vector1) a -> LinearMap (Vector2 a) (Vector1 a)
>   Mat22 :: (Num a, ConjugateSymmetric a) => (Vector2 :*: Vector2) a -> LinearMap (Vector2 a) (Vector2 a)
>   Mat23 :: (Num a, ConjugateSymmetric a) => (Vector2 :*: Vector3) a -> LinearMap (Vector2 a) (Vector3 a)
>   Mat24 :: (Num a, ConjugateSymmetric a) => (Vector2 :*: Vector4) a -> LinearMap (Vector2 a) (Vector4 a)
>   Mat31 :: (Num a, ConjugateSymmetric a) => (Vector3 :*: Vector1) a -> LinearMap (Vector3 a) (Vector1 a)
>   Mat32 :: (Num a, ConjugateSymmetric a) => (Vector3 :*: Vector2) a -> LinearMap (Vector3 a) (Vector2 a)
>   Mat33 :: (Num a, ConjugateSymmetric a) => (Vector3 :*: Vector3) a -> LinearMap (Vector3 a) (Vector3 a)
>   Mat34 :: (Num a, ConjugateSymmetric a) => (Vector3 :*: Vector4) a -> LinearMap (Vector3 a) (Vector4 a)
>   Mat41 :: (Num a, ConjugateSymmetric a) => (Vector4 :*: Vector1) a -> LinearMap (Vector4 a) (Vector1 a)
>   Mat42 :: (Num a, ConjugateSymmetric a) => (Vector4 :*: Vector2) a -> LinearMap (Vector4 a) (Vector2 a)
>   Mat43 :: (Num a, ConjugateSymmetric a) => (Vector4 :*: Vector3) a -> LinearMap (Vector4 a) (Vector3 a)
>   Mat44 :: (Num a, ConjugateSymmetric a) => (Vector4 :*: Vector4) a -> LinearMap (Vector4 a) (Vector4 a)
>   MatInd :: (Num a,ConjugateSymmetric a,Universe row, Universe col) => ((->) row :*: (->) col) a -> LinearMap (row -> a) (col -> a)
>   MatInd1 :: (Num a,ConjugateSymmetric a,Universe row) => ((->) row :*: Vector1) a -> LinearMap (row -> a) (Vector1 a)
>   MatInd2 :: (Num a,ConjugateSymmetric a,Universe row) => ((->) row :*: Vector2) a -> LinearMap (row -> a) (Vector2 a)
>   MatInd3 :: (Num a,ConjugateSymmetric a,Universe row) => ((->) row :*: Vector3) a -> LinearMap (row -> a) (Vector3 a)
>   MatInd4 :: (Num a,ConjugateSymmetric a, Universe row) => ((->) row :*: Vector4) a -> LinearMap (row -> a) (Vector4 a)
>   Mat1Ind :: (Num a,ConjugateSymmetric a,Universe col) => (Vector1 :*: (->) col) a -> LinearMap (Vector1 a) (col -> a)
>   Mat2Ind :: (Num a,ConjugateSymmetric a,Universe col) => (Vector2 :*: (->) col) a -> LinearMap (Vector2 a) (col -> a)
>   Mat3Ind :: (Num a,ConjugateSymmetric a,Universe col) => (Vector3 :*: (->) col) a -> LinearMap (Vector3 a) (col -> a)
>   Mat4Ind :: (Num a,ConjugateSymmetric a,Universe col) => (Vector4 :*: (->) col) a -> LinearMap (Vector4 a) (col -> a)
>   Mat1D :: (Num a, ConjugateSymmetric a) => (Vector1 :*: Dual) (f a) -> LinearMap (Vector1 (f a)) (Dual (f a))
>   Mat2D :: (Num a, ConjugateSymmetric a) => (Vector2 :*: Dual) (f a) -> LinearMap (Vector2 (f a)) (Dual (f a))
>   Mat3D :: (Num a, ConjugateSymmetric a) => (Vector3 :*: Dual) (f a) -> LinearMap (Vector3 (f a)) (Dual (f a))
>   Mat4D :: (Num a, ConjugateSymmetric a) => (Vector4 :*: Dual) (f a) -> LinearMap (Vector4 (f a)) (Dual (f a))
>   MatD1 :: (Num a, ConjugateSymmetric a) => (Dual :*: Vector1) a -> LinearMap (Dual a) (Vector1 a)
>   MatD2 :: (Num a, ConjugateSymmetric a) => (Dual :*: Vector2) a -> LinearMap (Dual a) (Vector2 a)
>   MatD3 :: (Num a, ConjugateSymmetric a) => (Dual :*: Vector3) a -> LinearMap (Dual a) (Vector3 a)
>   MatD4 :: (Num a, ConjugateSymmetric a) => (Dual :*: Vector4) a -> LinearMap (Dual a) (Vector4 a)
>   MatDD :: (Num a, ConjugateSymmetric a) => (Dual :*: Dual) (f a) -> LinearMap (Dual (f a)) (Dual (f a))
>   MatSS :: (Num a, ConjugateSymmetric a, InnerProductSpace (Stream a)) =>(Stream :*: Stream) a -> LinearMap (Stream a) (Stream a) -- Schauder basis
>   MatS1 :: (Num a, ConjugateSymmetric a, InnerProductSpace (Stream a)) =>(Stream :*: Vector1) a -> LinearMap (Stream a) (Vector1 a)
>   MatS2 :: (Num a, ConjugateSymmetric a, InnerProductSpace (Stream a)) =>(Stream :*: Vector2) a -> LinearMap (Stream a) (Vector2 a)
>   MatS3 :: (Num a, ConjugateSymmetric a, InnerProductSpace (Stream a)) =>(Stream :*: Vector3) a -> LinearMap (Stream a) (Vector3 a)
>   MatS4 :: (Num a, ConjugateSymmetric a, InnerProductSpace (Stream a)) =>(Stream :*: Vector4) a -> LinearMap (Stream a) (Vector4 a)
>   Mat1S :: (Num a, ConjugateSymmetric a, InnerProductSpace (Stream a)) =>(Vector1 :*: Stream) a -> LinearMap (Vector1 a) (Stream a)
>   Mat2S :: (Num a, ConjugateSymmetric a, InnerProductSpace (Stream a)) =>(Vector2 :*: Stream) a -> LinearMap (Vector2 a) (Stream a)
>   Mat3S :: (Num a, ConjugateSymmetric a, InnerProductSpace (Stream a)) =>(Vector3 :*: Stream) a -> LinearMap (Vector3 a) (Stream a)
>   Mat4S :: (Num a, ConjugateSymmetric a, InnerProductSpace (Stream a)) =>(Vector4 :*: Stream) a -> LinearMap (Vector4 a) (Stream a)

>-- | Specialization of 'linear' from Linearizable class to LinearMap.
>-- converts from matrix representation to linear map representation.
>linear_map :: (Linearizable LinearMap (:*:) f g a) => (f :*: g) a -> f a :-> g a
>linear_map = linear


Tensoring :: (a -> b -> c) -> LinearMap (f a, g b) (f c ⮾ g c)
Bilinear :: (a -> LinearMap b c) -> (b -> LinearMap a c) -> LinearMap (a ⮾ b) c

matrix_bilinear ::
   (LinearTransform g (f :*: h) a, Linearizable LinearMap (:*:) f h a,
    LinearTransform f (g :*: h) a, Linearizable LinearMap (:*:) g h a)
 =>  (f :*: (g :*: h)) a -> (g :*: (f :*: h)) a -> (f a, g a) :-> h a
matrix_bilinear b c = Bilinear (\a -> linear $ b <<*> a) (\a' -> linear $ c <<*> a')

-- Unicode character "CIRCLED X".
type (a ⮾ b) = Tensor a b

tensor :: (Functor f, Functor g) => (f a, g b) -> f a ⮾ g b
tensor (a,b) = Tensor $ matrix (,) a b

>instance (PpShowVerticalF f, LinearTransform f g a, Linearizable LinearMap (:*:) f g a, PpShowF g, PpShow a, Diagonalizable f a) => PpShow (LinearMap (f a) (g a)) where
>  pp x = pp $ fromLinear x

>instance (Diagonalizable f a, Linearizable LinearMap (:*:) f g a, LinearTransform f g a, Show ((f :*: g) a))
> => Show (LinearMap (f a) (g a)) where
>  showsPrec i x = showsPrec i (fromLinear x)

>instance (Eq (f (g a)), Linearizable LinearMap (:*:) f g a) => Eq (f a :-> g a) where
>  f == g = fromLinear f == fromLinear g

>-- <https://en.wikipedia.org/wiki/Linear_map>
>instance (Applicative f, Linearizable LinearMap (:*:) g g a, Diagonalizable g a, Applicative g, VectorSpace (f a), VectorSpace (g a), Linearizable LinearMap (:*:) f g a, Num a)
> => VectorSpace (LinearMap (f a) (g a)) where
>  type Scalar (LinearMap (f a) (g a)) = a
>  vzero = linear $ matrix (+) vzero vzero
>  vnegate x = arr_linear vnegate . x
>  (%+) = linear_map_plus
>  (%*) = linear_map_scalar_product

>linear_map_negate :: (Num a, Functor f, Functor g, Linearizable LinearMap (:*:) f g a) => f a :-> g a -> f a :-> g a
>linear_map_negate = linear . fmap negate . fromLinear

>linear_map_plus :: (Num a, Applicative f, Applicative g, Linearizable LinearMap (:*:) f g a)
> => f a :-> g a -> f a :-> g a -> f a :-> g a
>linear_map_plus f g = linear $ liftA2 (+) (fromLinear f) (fromLinear g)
> 
>linear_map_scalar_product :: (Num a, Linearizable LinearMap (:*:) f g a, Functor g, Functor f)
> => a -> f a :-> g a -> f a :-> g a
>linear_map_scalar_product a b = linear $ fmap (a*) (fromLinear b)

>-- | NOTICE: Linearizable instances for matrices that have similar dimensions are special.
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector1 Vector1 a where {
> fromLinear (Mat11 x) = x ; fromLinear MatIdentity = identity ; linear = Mat11 }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector1 Vector2 a where { fromLinear (Mat12 x) = x ; linear = Mat12 }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector1 Vector3 a where { fromLinear (Mat13 x) = x ; linear = Mat13 }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector1 Vector4 a where { fromLinear (Mat14 x) = x ; linear = Mat14 }

>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector2 Vector1 a where { fromLinear (Mat21 x) = x ; linear = Mat21 }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector2 Vector2 a where { fromLinear (Mat22 x) = x ; fromLinear MatIdentity = identity ; linear = Mat22 }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector2 Vector3 a where { fromLinear (Mat23 x) = x ; linear = Mat23 }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector2 Vector4 a where { fromLinear (Mat24 x) = x ; linear = Mat24 }
>
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector3 Vector1 a where { fromLinear (Mat31 x) = x ; linear = Mat31 }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector3 Vector2 a where { fromLinear (Mat32 x) = x ; linear = Mat32 }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector3 Vector3 a where { fromLinear (Mat33 x) = x ; fromLinear MatIdentity = identity ; linear = Mat33 }
>instance (Num a, ConjugateSymmetric a) =>  Linearizable LinearMap (:*:) Vector3 Vector4 a where { fromLinear (Mat34 x) = x ; linear = Mat34 }
>
>instance (Num a, ConjugateSymmetric a) =>  Linearizable LinearMap (:*:) Vector4 Vector1 a where { fromLinear (Mat41 x) = x ; linear = Mat41 }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector4 Vector2 a where { fromLinear (Mat42 x) = x ; linear = Mat42 }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector4 Vector3 a where { fromLinear (Mat43 x) = x ; linear = Mat43 }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector4 Vector4 a where { fromLinear (Mat44 x) = x ; fromLinear MatIdentity = identity ; linear = Mat44 }
>instance (Diagonalizable ((->) row) a, Num a,ConjugateSymmetric a,Universe row, Universe col) => Linearizable LinearMap (:*:) ((->) row) ((->) col) a where { fromLinear (MatInd x) = x; fromLinear MatIdentity = identity ; linear = MatInd }
>instance (Num a,ConjugateSymmetric a,Universe col) => Linearizable LinearMap (:*:) Vector1 ((->) col) a where { fromLinear (Mat1Ind x) = x ; linear = Mat1Ind }
>instance (Num a,ConjugateSymmetric a,Universe col) => Linearizable LinearMap (:*:) Vector2 ((->) col) a where { fromLinear (Mat2Ind x) = x ; linear = Mat2Ind }
>instance (Num a,ConjugateSymmetric a,Universe col) => Linearizable LinearMap (:*:) Vector3 ((->) col) a where { fromLinear (Mat3Ind x) = x ; linear = Mat3Ind }
>instance (Num a,ConjugateSymmetric a,Universe col) =>Linearizable LinearMap (:*:) Vector4 ((->) col) a where { fromLinear (Mat4Ind x) = x ; linear = Mat4Ind }
>instance (Num a, ConjugateSymmetric a, Universe row) => Linearizable LinearMap (:*:) ((->) row) Vector1 a where { fromLinear (MatInd1 x) = x ; linear = MatInd1 }
>instance (Num a, ConjugateSymmetric a, Universe row) => Linearizable LinearMap (:*:) ((->) row) Vector2 a where { fromLinear (MatInd2 x) = x ; linear = MatInd2 }
>instance (Num a, ConjugateSymmetric a, Universe row) => Linearizable LinearMap (:*:) ((->) row) Vector3 a where { fromLinear (MatInd3 x) = x ; linear = MatInd3 }
>instance (Num a, ConjugateSymmetric a, Universe row) => Linearizable LinearMap (:*:) ((->) row) Vector4 a where { fromLinear (MatInd4 x) = x ; linear = MatInd4 }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Dual Vector1 a where { fromLinear (MatD1 x) = x ; linear = MatD1 }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Dual Vector2 a where { fromLinear (MatD2 x) = x ; linear = MatD2 }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Dual Vector3 a where { fromLinear (MatD3 x) = x ; linear = MatD3 }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Dual Vector4 a where { fromLinear (MatD4 x) = x ; linear = MatD4 }
>instance (Num a, ConjugateSymmetric a, Scalar (f a) ~ a) => Linearizable LinearMap (:*:) Vector1 Dual (f a) where { fromLinear (Mat1D x) = x ; linear = Mat1D }
>instance (Num a, ConjugateSymmetric a, Scalar (f a) ~ a) => Linearizable LinearMap (:*:) Vector2 Dual (f a) where { fromLinear (Mat2D x) = x ; linear = Mat2D }
>instance (Num a, ConjugateSymmetric a, Scalar (f a) ~ a) => Linearizable LinearMap (:*:) Vector3 Dual (f a) where { fromLinear (Mat3D x) = x ; linear = Mat3D }
>instance (Num a, ConjugateSymmetric a, Scalar (f a) ~ a) => Linearizable LinearMap (:*:) Vector4 Dual (f a) where { fromLinear (Mat4D x) = x ; linear = Mat4D }
>instance (Num a, ConjugateSymmetric a, Diagonalizable Dual (f a)) => Linearizable LinearMap (:*:) Dual Dual (f a) where { fromLinear (MatDD x) = x ; fromLinear MatIdentity = identity ; linear = MatDD }
>instance (Num a, ConjugateSymmetric a, Diagonalizable Stream a, InnerProductSpace (Stream a)) => Linearizable LinearMap (:*:) Stream Stream a where { fromLinear (MatSS x) = x ; fromLinear MatIdentity = identity ; linear = MatSS }
>instance (Num a, ConjugateSymmetric a,InnerProductSpace (Stream a)) => Linearizable LinearMap (:*:) Stream Vector1 a where { fromLinear (MatS1 x) = x ; linear = MatS1 }
>instance (Num a, ConjugateSymmetric a,InnerProductSpace (Stream a)) => Linearizable LinearMap (:*:) Stream Vector2 a where { fromLinear (MatS2 x) = x ; linear = MatS2 }
>instance (Num a, ConjugateSymmetric a,InnerProductSpace (Stream a)) => Linearizable LinearMap (:*:) Stream Vector3 a where { fromLinear (MatS3 x) = x ; linear = MatS3 }
>instance (Num a, ConjugateSymmetric a,InnerProductSpace (Stream a)) => Linearizable LinearMap (:*:) Stream Vector4 a where { fromLinear (MatS4 x) = x ; linear = MatS4 }
>instance (Num a, ConjugateSymmetric a,InnerProductSpace (Stream a)) => Linearizable LinearMap (:*:) Vector1 Stream a where { fromLinear (Mat1S x) = x ; linear = Mat1S }
>instance (Num a, ConjugateSymmetric a,InnerProductSpace (Stream a)) => Linearizable LinearMap (:*:) Vector2 Stream a where { fromLinear (Mat2S x) = x ; linear = Mat2S }
>instance (Num a, ConjugateSymmetric a,InnerProductSpace (Stream a)) => Linearizable LinearMap (:*:) Vector3 Stream a where { fromLinear (Mat3S x) = x ; linear = Mat3S }
>instance (Num a, ConjugateSymmetric a,InnerProductSpace (Stream a)) => Linearizable LinearMap (:*:) Vector4 Stream a where { fromLinear (Mat4S x) = x ; linear = Mat4S }

>-- | Data type for dual vectors. <https://en.wikipedia.org/wiki/Dual_space>
>data Dual v where
>     Covector :: (f a :-> Vector1 a) -> Dual (f a)
>  deriving (Typeable)

>instance (Num a, ConjugateSymmetric a, Linearizable LinearMap (:*:) f Vector1 a,
> Diagonalizable f a,
> LinearTransform f Vector1 a) => VectorSpace (Dual (f a)) where
>   type Scalar (Dual (f a)) = Scalar (f a)
>   vzero = Covector $ arr_linear $ const 0
>   vnegate (Covector f) = Covector $ arr_linear negate . f
>   (Covector f) %+ (Covector g) = Covector $ arr_linear $ \x -> f -!< x %+ g -!< x
>   a %* (Covector f) = Covector $ arr_linear $ \x -> a %* (f -!< x)

>instance (Universe a) => Foldable ((->) a) where
>   foldMap :: (Monoid m) => (b -> m) -> (a -> b) -> m
>   foldMap f g = foldMap (f . g) all_elements

>-- | Foldable instance requires input 'f a' of 'LinearMap (f a)' to be finite.
>instance Foldable (LinearMap (f a)) where
>   foldMap f MatIdentity = mempty
>   foldMap f (Mat11 (Matrix m)) = foldMap f m
>   foldMap f (Mat12 (Matrix m)) = foldMap f m
>   foldMap f (Mat13 (Matrix m)) = foldMap f m
>   foldMap f (Mat14 (Matrix m)) = foldMap f m
>   foldMap f (Mat21 (Matrix m)) = foldMap f m 
>   foldMap f (Mat22 (Matrix m)) = foldMap f m
>   foldMap f (Mat23 (Matrix m)) = foldMap f m
>   foldMap f (Mat24 (Matrix m)) = foldMap f m
>   foldMap f (Mat31 (Matrix m)) = foldMap f m
>   foldMap f (Mat32 (Matrix m)) = foldMap f m
>   foldMap f (Mat33 (Matrix m)) = foldMap f m
>   foldMap f (Mat34 (Matrix m)) = foldMap f m
>   foldMap f (Mat41 (Matrix m)) = foldMap f m
>   foldMap f (Mat42 (Matrix m)) = foldMap f m
>   foldMap f (Mat43 (Matrix m)) = foldMap f m
>   foldMap f (Mat44 (Matrix m)) = foldMap f m
>   foldMap f (MatInd (Matrix m)) = foldMap f m
>   foldMap f (MatInd1 (Matrix m)) = foldMap f m
>   foldMap f (MatInd2 (Matrix m)) = foldMap f m
>   foldMap f (MatInd3 (Matrix m)) = foldMap f m
>   foldMap f (MatInd4 (Matrix m)) = foldMap f m
>   foldMap f (Mat1Ind (Matrix m)) = foldMap f m
>   foldMap f (Mat2Ind (Matrix m)) = foldMap f m
>   foldMap f (Mat3Ind (Matrix m)) = foldMap f m
>   foldMap f (Mat4Ind (Matrix m)) = foldMap f m
>   foldMap f (Mat1D (Matrix m)) = foldMap f m
>   foldMap f (Mat2D (Matrix m)) = foldMap f m
>   foldMap f (Mat3D (Matrix m)) = foldMap f m
>   foldMap f (Mat4D (Matrix m)) = foldMap f m
>   foldMap f (Mat1S (Matrix m)) = foldMap f m
>   foldMap f (Mat2S (Matrix m)) = foldMap f m
>   foldMap f (Mat3S (Matrix m)) = foldMap f m
>   foldMap f (Mat4S (Matrix m)) = foldMap f m
>   foldMap f _ = error "foldMap for LinearMap (f a) supports only finite structures."

>infixr 0 :->
>type (:->) a b = LinearMap a b

>data (:<->:) v w where
>  (:<->:) :: v :-> w -> w :-> v -> v :<->: w

>type End a = LinearMap a a
>type Aut a = a :<->: a
>type GL a = a :<->: a

>instance Category (:<->:) where
>  id = id :<->: id
>  (f :<->: finv) . (g :<->: ginv) = (f . g) :<->: (ginv . finv)


>compose_linear :: v :-> w -> u :-> v -> u :-> w
>compose_linear MatIdentity f = f
>compose_linear f MatIdentity = f
>compose_linear (Mat11 a) (Mat11 b) = Mat11 (b %*% a)
>compose_linear (Mat12 a) (Mat11 b) = Mat12 (b %*% a)
>compose_linear (Mat13 a) (Mat11 b) = Mat13 (b %*% a)
>compose_linear (Mat14 a) (Mat11 b) = Mat14 (b %*% a)
>compose_linear (Mat11 a) (Mat21 b) = Mat21 (b %*% a)
>compose_linear (Mat12 a) (Mat21 b) = Mat22 (b %*% a)
>compose_linear (Mat13 a) (Mat21 b) = Mat23 (b %*% a)
>compose_linear (Mat14 a) (Mat21 b) = Mat24 (b %*% a)
>compose_linear (Mat11 a) (Mat31 b) = Mat31 (b %*% a)
>compose_linear (Mat12 a) (Mat31 b) = Mat32 (b %*% a)
>compose_linear (Mat13 a) (Mat31 b) = Mat33 (b %*% a)
>compose_linear (Mat14 a) (Mat31 b) = Mat34 (b %*% a)
>compose_linear (Mat11 a) (Mat41 b) = Mat41 (b %*% a)
>compose_linear (Mat12 a) (Mat41 b) = Mat42 (b %*% a)
>compose_linear (Mat13 a) (Mat41 b) = Mat43 (b %*% a)
>compose_linear (Mat14 a) (Mat41 b) = Mat44 (b %*% a)

>compose_linear (Mat21 a) (Mat12 b) = Mat11 (b %*% a)
>compose_linear (Mat22 a) (Mat12 b) = Mat12 (b %*% a)
>compose_linear (Mat23 a) (Mat12 b) = Mat13 (b %*% a)
>compose_linear (Mat24 a) (Mat12 b) = Mat14 (b %*% a)
>compose_linear (Mat21 a) (Mat22 b) = Mat21 (b %*% a)
>compose_linear (Mat22 a) (Mat22 b) = Mat22 (b %*% a)
>compose_linear (Mat23 a) (Mat22 b) = Mat23 (b %*% a)
>compose_linear (Mat24 a) (Mat22 b) = Mat24 (b %*% a)
>compose_linear (Mat21 a) (Mat32 b) = Mat31 (b %*% a)
>compose_linear (Mat22 a) (Mat32 b) = Mat32 (b %*% a)
>compose_linear (Mat23 a) (Mat32 b) = Mat33 (b %*% a)
>compose_linear (Mat24 a) (Mat32 b) = Mat34 (b %*% a)
>compose_linear (Mat21 a) (Mat42 b) = Mat41 (b %*% a)
>compose_linear (Mat22 a) (Mat42 b) = Mat42 (b %*% a)
>compose_linear (Mat23 a) (Mat42 b) = Mat43 (b %*% a)
>compose_linear (Mat24 a) (Mat42 b) = Mat44 (b %*% a)

>compose_linear (Mat31 a) (Mat13 b) = Mat11 (b %*% a)
>compose_linear (Mat32 a) (Mat13 b) = Mat12 (b %*% a)
>compose_linear (Mat33 a) (Mat13 b) = Mat13 (b %*% a)
>compose_linear (Mat34 a) (Mat13 b) = Mat14 (b %*% a)
>compose_linear (Mat31 a) (Mat23 b) = Mat21 (b %*% a)
>compose_linear (Mat32 a) (Mat23 b) = Mat22 (b %*% a)
>compose_linear (Mat33 a) (Mat23 b) = Mat23 (b %*% a)
>compose_linear (Mat34 a) (Mat23 b) = Mat24 (b %*% a)
>compose_linear (Mat31 a) (Mat33 b) = Mat31 (b %*% a)
>compose_linear (Mat32 a) (Mat33 b) = Mat32 (b %*% a)
>compose_linear (Mat33 a) (Mat33 b) = Mat33 (b %*% a)
>compose_linear (Mat34 a) (Mat33 b) = Mat34 (b %*% a)
>compose_linear (Mat31 a) (Mat43 b) = Mat41 (b %*% a)
>compose_linear (Mat32 a) (Mat43 b) = Mat42 (b %*% a)
>compose_linear (Mat33 a) (Mat43 b) = Mat43 (b %*% a)
>compose_linear (Mat34 a) (Mat43 b) = Mat44 (b %*% a)

>compose_linear (Mat41 a) (Mat14 b) = Mat11 (b %*% a)
>compose_linear (Mat42 a) (Mat14 b) = Mat12 (b %*% a)
>compose_linear (Mat43 a) (Mat14 b) = Mat13 (b %*% a)
>compose_linear (Mat44 a) (Mat14 b) = Mat14 (b %*% a)
>compose_linear (Mat41 a) (Mat24 b) = Mat21 (b %*% a)
>compose_linear (Mat42 a) (Mat24 b) = Mat22 (b %*% a)
>compose_linear (Mat43 a) (Mat24 b) = Mat23 (b %*% a)
>compose_linear (Mat44 a) (Mat24 b) = Mat24 (b %*% a)
>compose_linear (Mat41 a) (Mat34 b) = Mat31 (b %*% a)
>compose_linear (Mat42 a) (Mat34 b) = Mat32 (b %*% a)
>compose_linear (Mat43 a) (Mat34 b) = Mat33 (b %*% a)
>compose_linear (Mat44 a) (Mat34 b) = Mat34 (b %*% a)
>compose_linear (Mat41 a) (Mat44 b) = Mat41 (b %*% a)
>compose_linear (Mat42 a) (Mat44 b) = Mat42 (b %*% a)
>compose_linear (Mat43 a) (Mat44 b) = Mat43 (b %*% a)
>compose_linear (Mat44 a) (Mat44 b) = Mat44 (b %*% a)
>compose_linear (MatInd a) (MatInd b) = MatInd (b %*% a)
>compose_linear (Mat1Ind a) (MatInd1 b) = MatInd (b %*% a)
>compose_linear (Mat2Ind a) (MatInd2 b) = MatInd (b %*% a)
>compose_linear (Mat3Ind a) (MatInd3 b) = MatInd (b %*% a)
>compose_linear (Mat4Ind a) (MatInd4 b) = MatInd (b %*% a)
>compose_linear (MatInd a) (Mat1Ind b) = Mat1Ind (b %*% a)
>compose_linear (MatInd1 a) (Mat1Ind b) = Mat11 (b %*% a)
>compose_linear (MatInd1 a) (Mat2Ind b) = Mat21 (b %*% a)
>compose_linear (MatSS a) (MatSS b) = MatSS (b %*% a)
>compose_linear (MatS1 a) (Mat1S b) = Mat11 (b %*% a)
>compose_linear (MatS1 a) (Mat2S b) = Mat21 (b %*% a)
>compose_linear (MatS1 a) (Mat3S b) = Mat31 (b %*% a)
>compose_linear (MatS1 a) (Mat4S b) = Mat41 (b %*% a)
>compose_linear (MatS2 a) (Mat1S b) = Mat12 (b %*% a)
>compose_linear (MatS2 a) (Mat2S b) = Mat22 (b %*% a)
>compose_linear (MatS2 a) (Mat3S b) = Mat32 (b %*% a)
>compose_linear (MatS2 a) (Mat4S b) = Mat42 (b %*% a)
>compose_linear (MatS3 a) (Mat1S b) = Mat13 (b %*% a)
>compose_linear (MatS3 a) (Mat2S b) = Mat23 (b %*% a)
>compose_linear (MatS3 a) (Mat3S b) = Mat33 (b %*% a)
>compose_linear (MatS3 a) (Mat4S b) = Mat43 (b %*% a)
>compose_linear (MatS4 a) (Mat1S b) = Mat14 (b %*% a)
>compose_linear (MatS4 a) (Mat2S b) = Mat24 (b %*% a)
>compose_linear (MatS4 a) (Mat3S b) = Mat34 (b %*% a)
>compose_linear (MatS4 a) (Mat4S b) = Mat44 (b %*% a)

>instance Category LinearMap where
>  id = MatIdentity
>  (.) = compose_linear

>appLinear :: (LinearTransform f g a) => f a :-> g a -> f a -> g a
>appLinear MatIdentity x = x
>appLinear (Mat11 f) x = f <<*> x
>appLinear (Mat12 f) x = f <<*> x
>appLinear (Mat13 f) x = f <<*> x
>appLinear (Mat14 f) x = f <<*> x
>appLinear (Mat21 f) x = f <<*> x
>appLinear (Mat22 f) x = f <<*> x
>appLinear (Mat23 f) x = f <<*> x
>appLinear (Mat24 f) x = f <<*> x
>appLinear (Mat31 f) x = f <<*> x
>appLinear (Mat32 f) x = f <<*> x
>appLinear (Mat33 f) x = f <<*> x
>appLinear (Mat34 f) x = f <<*> x
>appLinear (Mat41 f) x = f <<*> x
>appLinear (Mat42 f) x = f <<*> x
>appLinear (Mat43 f) x = f <<*> x
>appLinear (Mat44 f) x = f <<*> x
>appLinear (MatInd f) x = f <<*> x
>appLinear (MatInd1 f) x = f <<*> x
>appLinear (MatInd2 f) x = f <<*> x
>appLinear (MatInd3 f) x = f <<*> x
>appLinear (MatInd4 f) x = f <<*> x
>appLinear (Mat1Ind f) x = f <<*> x
>appLinear (Mat2Ind f) x = f <<*> x
>appLinear (Mat3Ind f) x = f <<*> x
>appLinear (Mat4Ind f) x = f <<*> x
>appLinear (MatD1 f) x = f <<*> x
>appLinear (MatD2 f) x = f <<*> x
>appLinear (MatD3 f) x = f <<*> x
>appLinear (MatD4 f) x = f <<*> x
>appLinear (MatDD f) x = f <<*> x
>appLinear (MatSS f) x = f <<*> x
>appLinear (MatS1 f) x = f <<*> x
>appLinear (MatS2 f) x = f <<*> x
>appLinear (MatS3 f) x = f <<*> x
>appLinear (MatS4 f) x = f <<*> x
>appLinear (Mat1S f) x = f <<*> x
>appLinear (Mat2S f) x = f <<*> x
>appLinear (Mat3S f) x = f <<*> x
>appLinear (Mat4S f) x = f <<*> x

instance (forall b. VectorSpace (g b), forall b. VectorSpace (f b), Indexable f a, Num a, Scalar (g a) ~ a, Indexable g a, Scalar (f a) ~ Scalar (g a), LinearTransform f g a)
 => VectorSpace (f a :-> g a) where
   type Scalar (f a :-> g a) = a
   vzero = LinearMap Refl $ matrix (\_ _ -> 0) vzero vzero
   vnegate (LinearMap p f) = LinearMap p $ vnegate f
   a %* (LinearMap p f) = LinearMap p $ a %* f
   (LinearMap p f) %+ (LinearMap q g) = LinearMap (vec2_cast p q) $ f %+ g

>instance (Fractional a, ConjugateSymmetric a) => Fractional ((Vector2 :*: Vector2) a) where
>   recip = inverse_impl
>   fromRational = diagonal_matrix_impl . constant2 . fromRational

>instance (ConjugateSymmetric a, Fractional a) => LinearInvertible LinearMap Vector2 a
>instance (ConjugateSymmetric a, Num a) => LinearTraceable LinearMap Vector2 a

>linear_matrix_multiply :: (SupportsMatrixMultiplication f g h a,
>    Linearizable LinearMap (:*:) h f a,
>    Linearizable LinearMap (:*:) g h a,
>    Linearizable LinearMap (:*:) g f a
> ) => g a :-> h a -> h a :-> f a -> g a :-> f a
>linear_matrix_multiply f g = linear (fromLinear f %*% fromLinear g)

>is_invertible_matrix :: (Eq a, Num a,Traceable m a, LinearTraceable LinearMap m a)
> => (m a :-> m a) -> Bool
>is_invertible_matrix m = determinant m /= 0

>invert_matrix :: (Eq a, Num a,Invertible m a, LinearInvertible LinearMap m a
> ) => (m a) :-> (m a)
> -> Maybe ((m a) :-> (m a))
>invert_matrix m | is_invertible_matrix m = Just (inverse m)
>                | otherwise = Nothing

>diagonal_matrix :: (Transposable m m a, LinearTransform m m a, Diagonalizable m a,
> Linearizable LinearMap (:*:) m m a
> ) => m a -> m a :-> m a
>diagonal_matrix = linear . diagonal_matrix_impl

>linear_inverse_impl :: (Linearizable LinearMap (:*:) f g a, Linearizable LinearMap (:*:) g f a,
> Transposable f g a
> ) =>  (f :*: g) a -> g a :-> f a
>linear_inverse_impl m = linear $ transpose_impl m

>lin_outer :: (Linearizable LinearMap (:*:) f g a, Diagonalizable f a,
>      InnerProductSpace (f a), VectorSpace (g a),
>      Scalar (f a) ~ Scalar (g a))
>  => f a -> g a -> f a :-> g a
>lin_outer x y = arr_linear (outer x y)

>matrixLin :: (LinearTransform f g c, Linearizable LinearMap (:*:) f g c)
> => (a -> b -> c) -> f a -> g b -> f c :-> g c
>matrixLin f x y = linear $ matrix f x y

>linmatrix :: (Functor g, Functor f, LinearTransform ((,) (t a)) v (u a),
>  Linearizable LinearMap (:*:) f g (v (u a)))
> => ((t a,u a) :-> v (u a)) -> (f (t a), g (u a)) -> f (v (u a)) :-> g (v (u a))
>linmatrix f (ma, nb) = linear $ Matrix $
>   flip fmap ma $ \a ->
>   flip fmap nb $ \b -> f -!< (a, b)

>arrmatrix :: (FunctorArrow f arr, FunctorArrow g arr, ArrowApply arr)
>          => arr (a,b) c -> arr (f a, g b) ((f :*: g) c)
>arrmatrix f = proc (ma, nb) -> do
>    res <- amap (proc a -> amap (proc b -> f -< (a,b)) -<< nb) -<< ma
>    returnA -< Matrix res

>-- | <https://en.wikipedia.org/wiki/Bilinear_map>
>bilinear :: (VectorSpace ((f :*: g) a), VectorSpace (g a), a ~ Scalar (g a), Indexable f a, Indexable g a, Integral a,
>  Linearizable LinearMap (:*:) f g a)
>   => (f a -> f a -> g a) -> f a -> f a -> f a :-> g a
>bilinear f a b = linear (bilinear_impl f a b)

dual_map :: (LinearTransform f I a, Diagonalizable f Integer, a ~ Scalar (f a), a ~ Scalar (g a), VectorSpace (g a), VectorSpace (f a), Diagonalizable f a, LinearTransform f g a)
  => Dual (f a) -> g a -> LinearMap (f a) (g a)
dual_map (Covector (lm :: f a :-> Vector1 a)) v = arr_linear (\u -> (unI $ lm -!< u) %* v) (vector_dimension $ (indexable_indices :: f Integer))

amapMatrix :: (LinearTransform f g a, Linearizable arr (:*:) f g b)
 => (b :-> a) -> arr (f b) (g b) -> f a :-> g a
amapMatrix f x = linear $ fmap ((-!<) f) $ fromLinear x

linearizableArrow :: (LinearTransform f g a, Linearizable arr (:*:) f g a)
 => arr (f a) (g a) -> f a :-> g a
linearizableArrow = amapMatrix id

dual_lin_apply :: (LinearTransform f g c, LinearTransform f h c,
  LinearTransform g Vector1 c, Scalar (g c) ~ c)
 => f (Dual (g c)) -> (h :*: g) c -> f c :-> h c
dual_lin_apply f (Matrix g) = matrixLin (-!!<) (fmap bracketMap f) g

>linear_applicative :: (LinearTransform f g c, Linearizable LinearMap (:*:) f g c)
> => f (b -> c) -> g b -> f c :-> g c
>linear_applicative = matrixLin id

>linear_apply_map :: (LinearTransform f g (u c), LinearTransform t u c,
> Linearizable LinearMap (:*:) f g (u c))
>  => f (t c :-> u c) -> g (t c) -> f (u c) :-> g (u c)
>linear_apply_map = matrixLin (-!<)

>instance HasIdentityLinear Vector1 LinearMap where
>   mat_identity = Mat11 identity
>instance HasIdentityLinear Vector2 LinearMap where
>   mat_identity = Mat22 identity
>instance HasIdentityLinear Vector3 LinearMap where
>   mat_identity = Mat33 identity
>instance HasIdentityLinear Vector4 LinearMap where
>   mat_identity = Mat44 identity

>linear_proof :: LinearMap v w -> Scalar v :~: Scalar w
>linear_proof MatIdentity = id
>linear_proof (Mat11 _) = id
>linear_proof (Mat12 _) = id
>linear_proof (Mat13 _) = id
>linear_proof (Mat14 _) = id
>linear_proof (Mat21 _) = id
>linear_proof (Mat22 _) = id
>linear_proof (Mat23 _) = id
>linear_proof (Mat24 _) = id
>linear_proof (Mat31 _) = id
>linear_proof (Mat32 _) = id
>linear_proof (Mat33 _) = id
>linear_proof (Mat34 _) = id
>linear_proof (Mat41 _) = id
>linear_proof (Mat42 _) = id
>linear_proof (Mat43 _) = id
>linear_proof (Mat44 _) = id
>linear_proof (MatInd _) = id
>linear_proof (MatInd1 _) = id
>linear_proof (MatInd2 _) = id
>linear_proof (MatInd3 _) = id
>linear_proof (MatInd4 _) = id
>linear_proof (Mat1Ind _) = id
>linear_proof (Mat2Ind _) = id
>linear_proof (Mat3Ind _) = id
>linear_proof (Mat4Ind _) = id
>linear_proof (MatDD _) = id
>linear_proof (MatSS _) = id
>linear_proof (MatS1 _) = id
>linear_proof (MatS2 _) = id
>linear_proof (MatS3 _) = id
>linear_proof (MatS4 _) = id
>linear_proof (Mat1S _) = id
>linear_proof (Mat2S _) = id
>linear_proof (Mat3S _) = id
>linear_proof (Mat4S _) = id

>infixr 8 -!<
>infixr 8 -!!<

>(-!<) :: (LinearTransform f g a) => f a :-> g a -> f a -> g a
>(-!<) = appLinear

>fromLinear_impl :: (Diagonalizable f a) => f a :-> g a -> (f :*: g) a
>fromLinear_impl MatIdentity = identity
>fromLinear_impl (Mat11 x) = x
>fromLinear_impl (Mat12 x) = x
>fromLinear_impl (Mat13 x) = x
>fromLinear_impl (Mat14 x) = x
>fromLinear_impl (Mat21 x) = x
>fromLinear_impl (Mat22 x) = x
>fromLinear_impl (Mat23 x) = x
>fromLinear_impl (Mat24 x) = x
>fromLinear_impl (Mat31 x) = x
>fromLinear_impl (Mat32 x) = x
>fromLinear_impl (Mat33 x) = x
>fromLinear_impl (Mat34 x) = x
>fromLinear_impl (Mat41 x) = x
>fromLinear_impl (Mat42 x) = x
>fromLinear_impl (Mat43 x) = x
>fromLinear_impl (Mat44 x) = x
>fromLinear_impl (MatInd x) = x
>fromLinear_impl (MatInd1 x) = x
>fromLinear_impl (MatInd2 x) = x
>fromLinear_impl (MatInd3 x) = x
>fromLinear_impl (MatInd4 x) = x
>fromLinear_impl (Mat1Ind x) = x
>fromLinear_impl (Mat2Ind x) = x
>fromLinear_impl (Mat3Ind x) = x
>fromLinear_impl (Mat1D x) = x
>fromLinear_impl (Mat2D x) = x
>fromLinear_impl (Mat3D x) = x
>fromLinear_impl (Mat4D x) = x
>fromLinear_impl (MatD1 x) = x
>fromLinear_impl (MatD2 x) = x
>fromLinear_impl (MatD3 x) = x
>fromLinear_impl (MatD4 x) = x
>fromLinear_impl (MatSS x) = x
>fromLinear_impl (MatS1 x) = x
>fromLinear_impl (MatS2 x) = x
>fromLinear_impl (MatS3 x) = x
>fromLinear_impl (MatS4 x) = x
>fromLinear_impl (Mat1S x) = x
>fromLinear_impl (Mat2S x) = x
>fromLinear_impl (Mat3S x) = x
>fromLinear_impl (Mat4S x) = x


>linear_apply :: (LinearTransform m n a, Linearizable arr (:*:) m n a)
> => arr (m a) (n a) -> m a -> n a
>linear_apply f = (fromLinear f <<*>)

>linear_apply_inverse :: (LinearTransform m n a, Linearizable arr (:*:) m n a)
> => arr (m a) (n a) -> n a -> m a
>linear_apply_inverse f = (<*>> fromLinear f)

>-- | for linear maps that produce scalars
>(-!!<) :: (LinearTransform f Vector1 r) => f r :-> Vector1 r -> f r -> r
>f -!!< x = vector_element (f -!< x)

>-- <https://en.wikipedia.org/wiki/Outer_product?wprov=sfla1>
>linear_outer_product :: (LinearTransform f g a, InnerProductSpace (v a),
>   Linearizable LinearMap (:*:) f g a, Scalar (v a) ~ a)
> => f (v a) -> g (v a) -> f a :-> g a
>linear_outer_product a b = matrixLin (%.) a b

>mapInnerProduct :: (InnerProductSpace (n a), LinearTransform m n a) =>
>  m a :-> n a -> m a -> m a -> Scalar (n a)
>mapInnerProduct f x y = f -!< x %. f -!< y

>linear_invert :: (Transposable g f a, Linearizable LinearMap (:*:) f g a,
> Linearizable LinearMap (:*:) g f a
>   )
>  => (g a) :-> (f a) -> (f a) :-> (g a)
>linear_invert = transpose 

>(<<*>>) :: (LinearTransform f g a, InnerProductSpace (v a), Scalar (v a) ~ a,
> Linearizable LinearMap (:*:) f g a
> )
> => f (v a) -> g (v a) -> f a :-> g a
>(<<*>>) = linear_outer_product

linear_identity_indexable :: (Num a, LinearTransform f f a, Diagonalizable f a, Indexable f a) => f a :-> f a
linear_identity_indexable = linear $ identity_impl indexable_indices

>matrix_equalizer :: (Eq (Scalar (h a)), Eq (g (f a)),
>   Linearizable LinearMap (:*:) h f a,
>   Linearizable LinearMap (:*:) g h a,
>  SupportsMatrixMultiplication f g h a,
>                     Applicative g, Applicative f,
>                     Foldable g, Foldable f  -- ,
>                --     Scalar (f a) ~ Scalar (g (f a)),
>                --     Scalar (g (h a)) ~ Scalar (g (f a))
>               )
>                 => (g a :-> h a) -> (g a :-> h a) -> Prop (h a :-> f a)
>matrix_equalizer a b = Characteristic $ \v -> let
>     a' = fromLinear a
>     b' = fromLinear b
>     v' = fromLinear v
>   in a' %*% v' == b' %*% v'

>cov1 :: (ConjugateSymmetric a, Num a) => Vector1 (Dual (Vector1 a))
>cov1 = Vector1 (covector $ vector_element)

>instance (ConjugateSymmetric a, Num a) => ProjectionDual Vector1 Dual a where
>   projection_dual = cov1

instance (Num a, a ~ Scalar a) => FiniteDimensional (Vector1 a) Dual I LinearMap where
   finite = LinearMap Refl $ \ (Matrix (Covector f)) ->
               (f -!< (covector $ vector_element))

>instance (ConjugateSymmetric a,Num a) => Dualizable (Vector1 a) Dual where
>   covector = covector_impl
>   bracket = bracket_impl

>instance (ConjugateSymmetric a, Num a, Closed a) => VectorDerivative (Vector1 a) Dual LinearMap where
>   divergence f = partial_derivate1x (covector (vector_element . (-!<) f))
>   grad f = arr_linear $ \z -> Vector1 (partial_derivate1x f `bracket` z)
>   directional_derivative = directional_derivative_impl

>-- | <https://en.wikipedia.org/wiki/Directional_derivative>
>-- Note: the direction v is not guaranteed to be normalized.
>directional_derivative_prim :: (Infinitesimal str a, Scalar (v a) ~ a, VectorSpace (v a))
>  => v a -> (v a -> a) -> v a -> Closure str a
>directional_derivative_prim v f x = let
>  fx = f x
> in limit $ do
>   h <- epsilon_stream
>   return $ (1/h) * (f (x %+ h %* v) - fx)

>-- | <https://en.wikipedia.org/wiki/Directional_derivative>
>-- Note: the direction v is not guaranteed to be normalized.
>directional_derivative_impl :: (LinearTransform v Vector1 a, Closed a, VectorSpace (v a), Linearizable LinearMap (:*:) v Vector1 a, Diagonalizable v a, Scalar (v a) ~ a)
>  => v a -> Dual (v a) -> Dual (v a)
>directional_derivative_impl v (Covector f) = Covector $ arr_linear $
>   Vector1 . accumulation_point
>           . directional_derivative_prim v (vector_element . appLinear f)

>directional_derivative_impl_ ::
>  (a ~ Scalar (v a), LinearTransform v v a, InnerProductSpace (v a),
>   Linearizable LinearMap (:*:) v Vector1 a, Diagonalizable v a,
>   Dualizable (v a) d,
>   VectorDerivative (v a) d LinearMap)
>  => v a -> d (v a) -> d (v a)
>directional_derivative_impl_ v d = covector $ \x -> v %. (grad d -!< x)


-- | <https://en.wikipedia.org/wiki/Conjugate_transpose>
instance (ConjugateSymmetric a) => ConjugateSymmetric (Vector1 a :-> Vector1 a) where
   conj = amap conj . transpose

-- | <https://en.wikipedia.org/wiki/Dual_space>
instance StandardBasis ((Dual :*: Vector1) a) where
  unit_vectors = [Matrix (Covector $ linear $ Matrix $ Covector $ id_linear Proxy)]

>-- | see "Lawvere,Rosebrugh: Sets for mathematics", pg. 167.
>instance (Num a, ConjugateSymmetric a) => Monoid (Vector1 a :-> Vector1 a) where
>   mempty = linear identity
>   mappend a b = linear $ fromLinear a %*% fromLinear b

>instance (Num a, ConjugateSymmetric a) => Semigroup (Vector1 a :-> Vector1 a) where
>   a <> b = linear $ fromLinear a %*% fromLinear b

>partial_derivate1x :: (Closed a, Num a, ConjugateSymmetric a)
>              => Dual (Vector1 a) -> Dual (Vector1 a)
>partial_derivate1x (Covector f) = covector $ partial_derivate ch (vector_element . appLinear f)
>   where ch eps (Vector1 x) = Vector1 (x+eps)

>cov2 :: (ConjugateSymmetric a, Num a) => Vector2 (Dual (Vector2 a))
>cov2 = Vector2 (covector xcoord2) (covector ycoord2)

>instance (ConjugateSymmetric a, Num a) => ProjectionDual Vector2 Dual a where
>   projection_dual = cov2

instance (Num a) => FiniteDimensional (Vector2 a) Dual Vector1 LinearMap where
   finite = arr_linear $ \ (Matrix (Covector f)) -> let f' = appLinear f
             in Vector1 $ Vector2 (f' (covector xcoord2)) (f' (covector ycoord2))

>instance (Num a,ConjugateSymmetric a) => Dualizable (Vector2 a) Dual where
>   covector = covector_impl
>   bracket = bracket_impl

>instance (ConjugateSymmetric a, Infinitesimal Stream a, Closed a) => VectorDerivative (Vector2 a) Dual LinearMap where
>   divergence = divergence2
>   grad = grad2
>   directional_derivative = directional_derivative_impl

>divergence2 :: (ConjugateSymmetric a, Closed a, Fractional a) => LinearMap (Vector2 a) (Vector2 a) -> Dual (Vector2 a)
>divergence2 f = partial_derivate2x (linear_dual_2x f)
>             %+ partial_derivate2y (linear_dual_2y f)

>linear_dual_2x :: (Dualizable (f v) d, Diagonalizable f v, LinearTransform f Vector2 v, Linearizable LinearMap (:*:) f Vector1 v)
> => LinearMap (f v) (Vector2 v) -> d (f v)
>linear_dual_2x f = covector (xcoord2 . (-!<) f)
>
>linear_dual_2y :: (Dualizable (f v) d, Diagonalizable f v, LinearTransform f Vector2 v, Linearizable LinearMap (:*:) f Vector1 v)
> => LinearMap (f v) (Vector2 v) -> d (f v)
>linear_dual_2y f = covector (ycoord2 . (-!<) f)

>grad2 :: (ConjugateSymmetric a, Closed a, Fractional a) => Dual (Vector2 a) -> LinearMap (Vector2 a) (Vector2 a)
>grad2 f = arr_linear $ \z -> Vector2 (partial_derivate2x f `bracket` z)
>                                    (partial_derivate2y f `bracket` z)

>curl2 :: (ConjugateSymmetric a, VectorSpace a,Fractional a, Closed a) => LinearMap (Vector2 a) (Vector2 a) -> LinearMap (Vector2 a) (Vector2 a)
>curl2 f = arr_linear $ \z -> Vector2 (partial_derivate2y fx `bracket` z
>                     %- partial_derivate2x fy `bracket` z)
>                    (partial_derivate2x fy `bracket` z 
>                     %- partial_derivate2y fx `bracket` z)
>  where fx = linear_dual_2x f
>        fy = linear_dual_2y f

>instance (ConjugateSymmetric a, Infinitesimal Stream a, VectorSpace a,Closed a) => VectorCrossProduct (Vector2 a) LinearMap where
>  curl = curl2
>
>instance (Infinitesimal Stream a, Closed a) => VectorLaplacian (Vector2 a) LinearMap

>cov3 :: (Num a, ConjugateSymmetric a) => Vector3 (Dual (Vector3 a))
>cov3 = Vector3 (covector xcoord3) (covector ycoord3) (covector zcoord3)

>instance (Num a, ConjugateSymmetric a) => ProjectionDual Vector3 Dual a where
>   projection_dual = cov3

>-- | <https://en.wikipedia.org/wiki/Dual_space#Injection_into_the_double-dual>

instance (Num a, LinearTransform Dual Vector1 (Vector3 a), ConjugateSymmetric a)
 => FiniteDimensional (Vector3 a) Dual Vector1 (->) where
   finite (Matrix f) = Vector1 $ Vector3
                                    (f `bracket` (covector xcoord3))
                                    (f `bracket` (covector ycoord3))
                                    (f `bracket` (covector zcoord3)) 

>vector3_to_vec3 :: Vector3 a -> ThreeD -> a
>vector3_to_vec3 (Vector3 x y z) = svec3 x y z

>vec3_iso_obj :: Vector3 ThreeD
>vec3_iso_obj = Vector3 ThreeD0 ThreeD1 ThreeD2

>vec3_to_vector3 :: (ThreeD -> a) -> Vector3 a
>vec3_to_vector3 f = fmap f vec3_iso_obj

>vector3_natiso :: (->) ThreeD :<~>: Vector3
>vector3_natiso = NaturalIso (NatTrans vec3_to_vector3)
>                            (NatTrans vector3_to_vec3)

>instance (ConjugateSymmetric a,Num a) => Dualizable (Vector3 a) Dual where
>   covector = covector_impl
>   bracket = bracket_impl

>instance (ConjugateSymmetric a,Num a) => Dualizable (Vector4 a) Dual where
>   covector = covector_impl
>   bracket = bracket_impl

>instance (Closed a, Num a, ConjugateSymmetric a) => VectorDerivative (Vector3 a) Dual LinearMap where
>  divergence = divergence3
>  grad = grad3
>  directional_derivative = directional_derivative_impl

>instance (Closed a, Num a, ConjugateSymmetric a) => VectorCrossProduct (Vector3 a) LinearMap where
>  curl = curl3

>instance (Closed a, Num a, ConjugateSymmetric a) => VectorLaplacian (Vector3 a) LinearMap where
>  vector_laplace = vector_laplace3

>vector_laplace3 :: (a ~ Scalar (f a), VectorDerivative (f a) Dual LinearMap,
> LinearTransform f Vector3 a,
> LinearTransform f Vector1 a,
> Diagonalizable f a, Dualizable (f a) Dual,
> Linearizable LinearMap (:*:) f Vector3 a,
> Linearizable LinearMap (:*:) f Vector1 a
> ) =>
>  f a :-> (Vector3 a) -> f a :-> (Vector3 a)
>vector_laplace3 f = arr_linear $ \x -> Vector3
> ((laplace $ linear_dual_3x f) `bracket` x)
> ((laplace $ linear_dual_3y f) `bracket` x)
> ((laplace $ linear_dual_3z f) `bracket` x)

>-- | <https://en.wikipedia.org/wiki/Divergence>
>divergence3 :: (Closed a, ConjugateSymmetric a, Num a)
> => (Vector3 a) :-> (Vector3 a) -> Dual (Vector3 a)
>divergence3 f = partial_derivate3x (linear_dual_3x f)
>              %+ partial_derivate3y (linear_dual_3y f)
>              %+ partial_derivate3z (linear_dual_3z f)

>-- | <https://en.wikipedia.org/wiki/Gradient>
>grad3 :: (Num s,ConjugateSymmetric s,Closed s) => Dual (Vector3 s) -> (Vector3 s) :-> (Vector3 s)
>grad3 f = arr_linear $ \z -> Vector3 (partial_derivate3x f `bracket` z)
>                                    (partial_derivate3y f `bracket` z)
>                                    (partial_derivate3z f `bracket` z)

>-- | <https://en.wikipedia.org/wiki/Laplace_operator>
>laplace3 :: (Num a, ConjugateSymmetric a,Closed a) => Dual (Vector3 a) -> Dual (Vector3 a)
>laplace3 f = divergence3 (grad3 f)


>-- | <https://en.wikipedia.org/wiki/Curl_(mathematics)>

>curl3 :: (ConjugateSymmetric a,Num a, Closed a) => (Vector3 a) :-> (Vector3 a)  -> (Vector3 a) :-> (Vector3 a)
>curl3 f = arr_linear $ \z -> Vector3 ((partial_derivate3y fz %- partial_derivate3z fy)
>                       `bracket` z)
>                    ((partial_derivate3z fx %- partial_derivate3x fz)
>                       `bracket` z)
>                    ((partial_derivate3x fy %- partial_derivate3y fx)
>                       `bracket` z)
>  where fx = linear_dual_3x f
>        fy = linear_dual_3y f
>        fz = linear_dual_3z f

>linear_dual_3x :: (Dualizable (f a) Dual, Diagonalizable f a,Linearizable LinearMap (:*:) f Vector1 a,LinearTransform f Vector3 a) => (f a) :-> (Vector3 a) -> Dual (f a)
>linear_dual_3x f = covector (xcoord3 . (-!<) f)

>linear_dual_3y :: (Dualizable (f a) Dual, Diagonalizable f a,Linearizable LinearMap (:*:) f Vector1 a,LinearTransform f Vector3 a) => (f a) :-> (Vector3 a) -> Dual (f a)
>linear_dual_3y f = covector (ycoord3 . (-!<) f)

>linear_dual_3z :: (Linearizable LinearMap (:*:) f Vector1 a,
>                  Diagonalizable f a, Dualizable (f a) Dual,
>                  LinearTransform f Vector3 a)
> => (f a) :-> (Vector3 a) -> Dual (f a)
>linear_dual_3z f = covector (zcoord3 . (-!<) f)

>partial_derivate2x :: (Fractional a,Closed a, ConjugateSymmetric a) 
>                   => Dual (Vector2 a) -> Dual (Vector2 a)
>partial_derivate2x (Covector f) = covector $ partial_derivate ch (vector_element . appLinear f)
>  where ch eps (Vector2 x y) = Vector2 (x+eps) y

>partial_derivate2y :: (Fractional a, Closed a, ConjugateSymmetric a)
>                   => Dual (Vector2 a) -> Dual (Vector2 a)
>partial_derivate2y (Covector f) = covector $ partial_derivate ch (vector_element . appLinear f)
>  where ch eps (Vector2 x y) = Vector2 x (y+eps)


>partial_derivate3x :: (ConjugateSymmetric a, Num a,Closed a)
>                   => Dual (Vector3 a) -> Dual (Vector3 a)
>partial_derivate3x (Covector f) = covector $ partial_derivate dx3 (vector_element . appLinear f)

>partial_derivate3y :: (ConjugateSymmetric a, Num a,Closed a) => Dual (Vector3 a) -> Dual (Vector3 a)
>partial_derivate3y (Covector f) = covector $ partial_derivate dy3 (vector_element . appLinear f)
          
>partial_derivate3z :: (ConjugateSymmetric a,Closed a, Num a) => Dual (Vector3 a) -> Dual (Vector3 a)
>partial_derivate3z (Covector f) = covector $ partial_derivate dz3 (vector_element . appLinear f)

>-- | \[\nabla_3\], three-dimensional partial derivate. Use Applicative.\<*\> for applying it.
>del3 :: (ConjugateSymmetric v,Num v,Closed v) => Vector3 (Dual (Vector3 v) -> Dual (Vector3 v))
>del3 = Vector3 partial_derivate3x partial_derivate3y partial_derivate3z


>-- outer product \[\nabla_3 \otimes \nabla_3\] operator for three-dimensional vectors.
>del3_squared :: (Num v, Closed v, ConjugateSymmetric v)
>  => (Vector3 :*: Vector3) (Dual (Vector3 v) -> Dual (Vector3 v))
>del3_squared = matrix (.) del3 del3

>hessian3 :: (Closed v, ConjugateSymmetric v, Num v)
> => Dual (Vector3 v) -> (Vector3 :*: Vector3) (Dual (Vector3 v))
>hessian3 f = matrix (\a b -> a (b f)) del3 del3

>-- | <https://en.wikipedia.org/wiki/Dual_space>
>instance (Num a, ConjugateSymmetric a) => StandardBasis ((Dual :*: Vector3) a) where
>  unit_vectors = [dual_unitvector3 i3,
>                  dual_unitvector3 j3,
>                  dual_unitvector3 k3]

>dual_unitvector3 :: (Num a, ConjugateSymmetric a) => Vector3 a -> (Dual :*: Vector3) a
>dual_unitvector3 v = Matrix (Covector $ Mat31 $ Matrix $ fmap Vector1 v)

>-- | this computes partial derivates of the scalar-valued 3D vector field
>-- along each variable simultaneously.
>-- \[\nabla f({\mathbb{v}}) = \frac{\partial \mathbb{f}}{\partial x}{\mathbb{i}}
>--                     + \frac{\partial \mathbb{f}}{\partial y}{\mathbb{j}}
>--                     + \frac{\partial \mathbb{f}}{\partial z}{\mathbb{k}}\]
>del_vector3 :: (Infinitesimal s a)
>  => (Vector3 a -> a) -> Vector3 a -> Vector3 (Closure s a)
>del_vector3 f (Vector3 x y z) = Vector3 (partial_derivate1_3 ff x y z)
>                                        (partial_derivate2_3 ff x y z)
>                                        (partial_derivate3_3 ff x y z)
>  where ff a b c = f (Vector3 a b c)

>instance DifferentialOperator Vector3 where
>   partial = del_partial3

>del_partial3 :: (DifferentiallyClosed a) => (Vector3 a -> a) -> Vector3 a -> Vector3 a
>del_partial3 f (Vector3 x y z) = Vector3 (partial1_3 ff x y z)
>                                         (partial2_3 ff x y z)
>                                         (partial3_3 ff x y z)
>  where ff a b c = f (Vector3 a b c)

>partial_derivate_vector3 :: (Infinitesimal s (Scalar a)
> , VectorSpace a, Num a, Limiting s a)
>   => (Vector3 (Scalar a) -> a) -> Vector3 (Scalar a) -> Closure s (Vector3 a)
>partial_derivate_vector3 f (Vector3 x y z) = Vector3Closure $
>      Vector3 (pd1 (callf f) x y z)
>              (pd2 (callf f) x y z)
>              (pd3 (callf f) x y z)
>   where
>      callf ff a b c = ff (Vector3 a b c)
>      pd1 ff a b c = limit $ epsilon_stream >>= \ eps ->
>            return $ (1/(2*eps)) %* (ff (a+eps) b c - ff (a-eps) b c)
>      pd2 ff a b c = limit $ epsilon_stream >>= \ eps ->
>            return $ (1/(2*eps)) %* (ff a (b+eps) c - ff a (b-eps) c)
>      pd3 ff a b c = limit $ epsilon_stream >>= \ eps ->
>            return $ (1/(2*eps)) %* (ff a b (c+eps) - ff a b (c-eps))


>-- | partial derivate a function defined for each coordinate along
>-- each dimension of three-dimensional vector.
>pderive3 :: (Closed a, Num a)
>   => Vector3 (a -> a) -> Vector3 a -> Vector3 a
>pderive3 (Vector3 fx fy fz) (Vector3 x y z) = Vector3
>   (partial_derive (+) fx x)
>   (partial_derive (+) fy y)
>   (partial_derive (+) fz z)

>vector_field_derivate :: (Closed a, Num a)
>  => (Vector3 a -> a) -> Vector3 a -> Vector3 a
>vector_field_derivate f v@(Vector3 x y z) = Vector3
>   (partial_derivate dx3 f v)
>   (partial_derivate dy3 f v)
>   (partial_derivate dz3 f v)

>mat_bind :: (Monad f) => (f :*: g) a -> (g a -> (f :*: h) b) -> (f :*: h) b
>mat_bind (Matrix m) f = Matrix $ m >>= (cells . f)

>cells_linear :: (Linearizable LinearMap (:*:) f g a) => (f a :-> g a) -> f (g a)
>cells_linear = cells . fromLinear

>lin_bind :: (Linearizable LinearMap (:*:) f g a,
>            Linearizable LinearMap (:*:) f h b, Monad f)
>  => f a :-> g a -> (g a -> f b :-> h b) -> f b :-> h b
>lin_bind m f = linear $ mat_bind (fromLinear m) (fromLinear . f)

>covector_impl :: (Linearizable LinearMap (:*:) f Vector1 b, Diagonalizable f b) => (f b -> b) -> Dual (f b)
>covector_impl f = Covector (arr_linear (Vector1 . f))

>linear_outer_product_ a b = arr_linear (Math.Matrix.Interface.outer a b)

>arr_linear :: (Linearizable arr (:*:) f g a, Diagonalizable f a)
>  => (f a -> g a) -> arr (f a) (g a)
>arr_linear f = linear $ functionMatrix f
>
>arr_natural :: (Linearizable arr (:*:) f g a, Diagonalizable f a) => f :~> g -> arr (f a) (g a)
>arr_natural (NatTrans m) = arr_linear m

>bracketMap :: Dual (f a) -> f a :-> Vector1 a
>bracketMap (Covector f) = f

>bracket_impl :: (LinearTransform f Vector1 a) => Dual (f a) -> f a -> a
>bracket_impl x = vector_element . appLinear (bracketMap x)
