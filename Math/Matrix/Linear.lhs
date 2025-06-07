>-- -*- coding: utf-8 -*-
>{-# LANGUAGE Safe, GADTs, RankNTypes, TypeOperators, ScopedTypeVariables #-}
>{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
>{-# LANGUAGE FlexibleContexts, TypeFamilies, PolyKinds #-}
>{-# LANGUAGE UndecidableInstances, Arrows #-}
>{-# LANGUAGE DeriveGeneric, StandaloneDeriving, InstanceSigs #-}
>{-# LANGUAGE TypeApplications, QuantifiedConstraints, AllowAmbiguousTypes #-}
>module Math.Matrix.Linear where
>import safe Prelude hiding (id,(.))
>import safe Control.Category
>import safe Control.Arrow
>import safe Control.Applicative
>import safe Data.Typeable
>import qualified Data.Foldable
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
>import safe Math.Matrix.SIMD
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
>   MatInd :: (InnerProductSpaceFunctor ((->) col) a,InnerProductSpaceFunctor ((->) row) a,
>            ConjugateSymmetric a,
>            Universe row, Universe col) => ((->) row :*: (->) col) a -> LinearMap (row -> a) (col -> a)
>   MatInd1 :: (InnerProductSpaceFunctor ((->) row) a,ConjugateSymmetric a,Universe row) => ((->) row :*: Vector1) a -> LinearMap (row -> a) (Vector1 a)
>   MatInd2 :: (InnerProductSpaceFunctor ((->) row) a,ConjugateSymmetric a,Universe row) => ((->) row :*: Vector2) a -> LinearMap (row -> a) (Vector2 a)
>   MatInd3 :: (InnerProductSpaceFunctor ((->) row) a,ConjugateSymmetric a,Universe row) => ((->) row :*: Vector3) a -> LinearMap (row -> a) (Vector3 a)
>   MatInd4 :: (InnerProductSpaceFunctor ((->) row) a,ConjugateSymmetric a, Universe row) => ((->) row :*: Vector4) a -> LinearMap (row -> a) (Vector4 a)
>   Mat1Ind :: (InnerProductSpaceFunctor ((->) col) a,ConjugateSymmetric a,Universe col) => (Vector1 :*: (->) col) a -> LinearMap (Vector1 a) (col -> a)
>   Mat2Ind :: (InnerProductSpaceFunctor ((->) col) a,ConjugateSymmetric a,Universe col) => (Vector2 :*: (->) col) a -> LinearMap (Vector2 a) (col -> a)
>   Mat3Ind :: (InnerProductSpaceFunctor ((->) col) a,ConjugateSymmetric a,Universe col) => (Vector3 :*: (->) col) a -> LinearMap (Vector3 a) (col -> a)
>   Mat4Ind :: (InnerProductSpaceFunctor ((->) col) a,ConjugateSymmetric a,Universe col) => (Vector4 :*: (->) col) a -> LinearMap (Vector4 a) (col -> a)
>   Mat1D :: (Num a, ConjugateSymmetric a) => (Vector1 :*: Dual) (f a) -> LinearMap (Vector1 (f a)) (Dual (f a))
>   Mat2D :: (Num a, ConjugateSymmetric a) => (Vector2 :*: Dual) (f a) -> LinearMap (Vector2 (f a)) (Dual (f a))
>   Mat3D :: (Num a, ConjugateSymmetric a) => (Vector3 :*: Dual) (f a) -> LinearMap (Vector3 (f a)) (Dual (f a))
>   Mat4D :: (Num a, ConjugateSymmetric a) => (Vector4 :*: Dual) (f a) -> LinearMap (Vector4 (f a)) (Dual (f a))
>   MatD1 :: (Scalar a ~ a, Num a, ConjugateSymmetric a) => (Dual :*: Vector1) a -> LinearMap (Dual a) (Vector1 a)
>   MatD2 :: (Scalar a ~ a, Num a, ConjugateSymmetric a) => (Dual :*: Vector2) a -> LinearMap (Dual a) (Vector2 a)
>   MatD3 :: (Scalar a ~ a, Num a, ConjugateSymmetric a) => (Dual :*: Vector3) a -> LinearMap (Dual a) (Vector3 a)
>   MatD4 :: (Scalar a ~ a, Num a, ConjugateSymmetric a) => (Dual :*: Vector4) a -> LinearMap (Dual a) (Vector4 a)
>   MatDD :: (Scalar (f a) ~ f a,Num a, ConjugateSymmetric a) => (Dual :*: Dual) (f a) -> LinearMap (Dual (f a)) (Dual (f a))
>   MatSS :: (Num a, ConjugateSymmetric a, InnerProductSpaceFunctor Stream a) =>(Stream :*: Stream) a -> LinearMap (Stream a) (Stream a) -- Schauder basis
>   MatS1 :: (Num a, ConjugateSymmetric a, InnerProductSpaceFunctor Stream a) =>(Stream :*: Vector1) a -> LinearMap (Stream a) (Vector1 a)
>   MatS2 :: (Num a, ConjugateSymmetric a, InnerProductSpaceFunctor Stream a) =>(Stream :*: Vector2) a -> LinearMap (Stream a) (Vector2 a)
>   MatS3 :: (Num a, ConjugateSymmetric a, InnerProductSpaceFunctor Stream a) =>(Stream :*: Vector3) a -> LinearMap (Stream a) (Vector3 a)
>   MatS4 :: (Num a, ConjugateSymmetric a, InnerProductSpaceFunctor Stream a) =>(Stream :*: Vector4) a -> LinearMap (Stream a) (Vector4 a)
>   Mat1S :: (Num a, ConjugateSymmetric a, InnerProductSpaceFunctor Stream a) =>(Vector1 :*: Stream) a -> LinearMap (Vector1 a) (Stream a)
>   Mat2S :: (Num a, ConjugateSymmetric a, InnerProductSpaceFunctor Stream a) =>(Vector2 :*: Stream) a -> LinearMap (Vector2 a) (Stream a)
>   Mat3S :: (Num a, ConjugateSymmetric a, InnerProductSpaceFunctor Stream a) =>(Vector3 :*: Stream) a -> LinearMap (Vector3 a) (Stream a)
>   Mat4S :: (Num a, ConjugateSymmetric a, InnerProductSpaceFunctor Stream a) =>(Vector4 :*: Stream) a -> LinearMap (Vector4 a) (Stream a)

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

>tensor :: (Functor f, Functor g,
>          Linearizable arr (:*:) f g c)
>        => ((a,b) -> c) -> (f a, g b) -> arr (f c) (g c)
>tensor f (a,b) = linear $ matrix (\a' b' -> f (a', b')) a b

>lin_tensor :: (Linearizable arr (:*:) f g (h b),
>  LinearTransform ((,) a) h c,
>  Functor f, Functor g)
> => ((a,c) :-> h b) -> (f a, g c) -> arr (f (h b)) (g (h b))
>lin_tensor f (a,b) = linear $ matrix (\a' b' -> appLinear f (a', b')) a b


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
>  vnegate x = arrLinear vnegate . x
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
>instance (InnerProductSpaceFunctor ((->) row) a, InnerProductSpaceFunctor ((->) col) a,
>         ConjugateSymmetric a,Diagonalizable ((->) row) a, Universe row, Universe col)
>  => Linearizable LinearMap (:*:) ((->) row) ((->) col) a where
>   fromLinear (MatInd x) = x
>   fromLinear MatIdentity = identity
>   linear = MatInd
>instance (InnerProductSpaceFunctor ((->) col) a,ConjugateSymmetric a,Universe col) => Linearizable LinearMap (:*:) Vector1 ((->) col) a where { fromLinear (Mat1Ind x) = x ; linear = Mat1Ind }
>instance (InnerProductSpaceFunctor ((->) col) a,ConjugateSymmetric a,Universe col) => Linearizable LinearMap (:*:) Vector2 ((->) col) a where { fromLinear (Mat2Ind x) = x ; linear = Mat2Ind }
>instance (InnerProductSpaceFunctor ((->) col) a,ConjugateSymmetric a,Universe col) => Linearizable LinearMap (:*:) Vector3 ((->) col) a where { fromLinear (Mat3Ind x) = x ; linear = Mat3Ind }
>instance (InnerProductSpaceFunctor ((->) col) a,ConjugateSymmetric a,Universe col) =>Linearizable LinearMap (:*:) Vector4 ((->) col) a where { fromLinear (Mat4Ind x) = x ; linear = Mat4Ind }
>instance (InnerProductSpaceFunctor ((->) row) a,ConjugateSymmetric a,Universe row) => Linearizable LinearMap (:*:) ((->) row) Vector1 a where { fromLinear (MatInd1 x) = x ; linear = MatInd1 }
>instance (InnerProductSpaceFunctor ((->) row) a, ConjugateSymmetric a,Universe row) => Linearizable LinearMap (:*:) ((->) row) Vector2 a where { fromLinear (MatInd2 x) = x ; linear = MatInd2 }
>instance (InnerProductSpaceFunctor ((->) row) a, ConjugateSymmetric a,Universe row) => Linearizable LinearMap (:*:) ((->) row) Vector3 a where { fromLinear (MatInd3 x) = x ; linear = MatInd3 }
>instance (InnerProductSpaceFunctor ((->) row) a, ConjugateSymmetric a,Universe row) => Linearizable LinearMap (:*:) ((->) row) Vector4 a where { fromLinear (MatInd4 x) = x ; linear = MatInd4 }
>instance (Scalar a ~ a, Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Dual Vector1 a where { fromLinear (MatD1 x) = x ; linear = MatD1 }
>instance (Scalar a ~ a, Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Dual Vector2 a where { fromLinear (MatD2 x) = x ; linear = MatD2 }
>instance (Scalar a ~ a, Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Dual Vector3 a where { fromLinear (MatD3 x) = x ; linear = MatD3 }
>instance (Scalar a ~ a, Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Dual Vector4 a where { fromLinear (MatD4 x) = x ; linear = MatD4 }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector1 Dual (f a) where { fromLinear (Mat1D x) = x ; linear = Mat1D }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector2 Dual (f a) where { fromLinear (Mat2D x) = x ; linear = Mat2D }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector3 Dual (f a) where { fromLinear (Mat3D x) = x ; linear = Mat3D }
>instance (Num a, ConjugateSymmetric a) => Linearizable LinearMap (:*:) Vector4 Dual (f a) where { fromLinear (Mat4D x) = x ; linear = Mat4D }
>instance (Scalar (f a) ~ f a, Num a, Num (f a), ConjugateSymmetric a, Diagonalizable Dual (f a)) => Linearizable LinearMap (:*:) Dual Dual (f a) where { fromLinear (MatDD x) = x ; fromLinear MatIdentity = identity ; linear = MatDD }
>instance (Num a, ConjugateSymmetric a, Diagonalizable Stream a, InnerProductSpaceFunctor Stream a)
> => Linearizable LinearMap (:*:) Stream Stream a where
>   fromLinear (MatSS x) = x
>   fromLinear MatIdentity = identity
>   linear = MatSS 
>instance (Num a, ConjugateSymmetric a,InnerProductSpaceFunctor Stream a) => Linearizable LinearMap (:*:) Stream Vector1 a where { fromLinear (MatS1 x) = x ; linear = MatS1 }
>instance (Num a, ConjugateSymmetric a,InnerProductSpaceFunctor Stream a) => Linearizable LinearMap (:*:) Stream Vector2 a where { fromLinear (MatS2 x) = x ; linear = MatS2 }
>instance (Num a, ConjugateSymmetric a,InnerProductSpaceFunctor Stream a) => Linearizable LinearMap (:*:) Stream Vector3 a where { fromLinear (MatS3 x) = x ; linear = MatS3 }
>instance (Num a, ConjugateSymmetric a,InnerProductSpaceFunctor Stream a) => Linearizable LinearMap (:*:) Stream Vector4 a where { fromLinear (MatS4 x) = x ; linear = MatS4 }
>instance (Num a, ConjugateSymmetric a,InnerProductSpaceFunctor Stream a) => Linearizable LinearMap (:*:) Vector1 Stream a where { fromLinear (Mat1S x) = x ; linear = Mat1S }
>instance (Num a, ConjugateSymmetric a,InnerProductSpaceFunctor Stream a) => Linearizable LinearMap (:*:) Vector2 Stream a where { fromLinear (Mat2S x) = x ; linear = Mat2S }
>instance (Num a, ConjugateSymmetric a,InnerProductSpaceFunctor Stream a) => Linearizable LinearMap (:*:) Vector3 Stream a where { fromLinear (Mat3S x) = x ; linear = Mat3S }
>instance (Num a, ConjugateSymmetric a,InnerProductSpaceFunctor Stream a) => Linearizable LinearMap (:*:) Vector4 Stream a where { fromLinear (Mat4S x) = x ; linear = Mat4S }

>-- | Data type for dual vectors. <https://en.wikipedia.org/wiki/Dual_space>
>data Dual v where
>     Covector :: (f a :-> Vector1 (Scalar (f a))) -> Dual (f a)
>  deriving (Typeable)


>instance (Num a, ConjugateSymmetric a, Linearizable LinearMap (:*:) f Vector1 a,
> Diagonalizable f a,
> LinearTransform f Vector1 a) => VectorSpace (Dual (f a)) where
>   type Scalar (Dual (f a)) = Scalar (f a)
>   vzero = Covector $ arrLinear $ const 0
>   vnegate (Covector f) = Covector $ arrLinear negate . f
>   (Covector f) %+ (Covector g) = Covector $ arrLinear $ \x -> f -!< x %+ g -!< x
>   a %* (Covector f) = Covector $ arrLinear $ \x -> a %* (f -!< x)

>instance (Universe a) => Foldable ((->) a) where
>   foldMap :: (Monoid m) => (b -> m) -> (a -> b) -> m
>   foldMap f g = foldMap (f . g) allElements

>-- | Foldable instance requires input 'f a' of 'LinearMap (f a)' to be finite.
>instance (Num a, Foldable f,Diagonalizable f a) => Foldable (LinearMap (f a)) where
>   foldMap f (MatIdentity :: LinearMap (f a) b) =
>     Data.Foldable.foldMap f (cells identity)
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

>composeLinear :: v :-> w -> u :-> v -> u :-> w
>composeLinear MatIdentity f = f
>composeLinear f MatIdentity = f
>composeLinear (Mat11 a) (Mat11 b) = Mat11 (b %*% a)
>composeLinear (Mat12 a) (Mat11 b) = Mat12 (b %*% a)
>composeLinear (Mat13 a) (Mat11 b) = Mat13 (b %*% a)
>composeLinear (Mat14 a) (Mat11 b) = Mat14 (b %*% a)
>composeLinear (Mat11 a) (Mat21 b) = Mat21 (b %*% a)
>composeLinear (Mat12 a) (Mat21 b) = Mat22 (b %*% a)
>composeLinear (Mat13 a) (Mat21 b) = Mat23 (b %*% a)
>composeLinear (Mat14 a) (Mat21 b) = Mat24 (b %*% a)
>composeLinear (Mat11 a) (Mat31 b) = Mat31 (b %*% a)
>composeLinear (Mat12 a) (Mat31 b) = Mat32 (b %*% a)
>composeLinear (Mat13 a) (Mat31 b) = Mat33 (b %*% a)
>composeLinear (Mat14 a) (Mat31 b) = Mat34 (b %*% a)
>composeLinear (Mat11 a) (Mat41 b) = Mat41 (b %*% a)
>composeLinear (Mat12 a) (Mat41 b) = Mat42 (b %*% a)
>composeLinear (Mat13 a) (Mat41 b) = Mat43 (b %*% a)
>composeLinear (Mat14 a) (Mat41 b) = Mat44 (b %*% a)

>composeLinear (Mat21 a) (Mat12 b) = Mat11 (b %*% a)
>composeLinear (Mat22 a) (Mat12 b) = Mat12 (b %*% a)
>composeLinear (Mat23 a) (Mat12 b) = Mat13 (b %*% a)
>composeLinear (Mat24 a) (Mat12 b) = Mat14 (b %*% a)
>composeLinear (Mat21 a) (Mat22 b) = Mat21 (b %*% a)
>composeLinear (Mat22 a) (Mat22 b) = Mat22 (b %*% a)
>composeLinear (Mat23 a) (Mat22 b) = Mat23 (b %*% a)
>composeLinear (Mat24 a) (Mat22 b) = Mat24 (b %*% a)
>composeLinear (Mat21 a) (Mat32 b) = Mat31 (b %*% a)
>composeLinear (Mat22 a) (Mat32 b) = Mat32 (b %*% a)
>composeLinear (Mat23 a) (Mat32 b) = Mat33 (b %*% a)
>composeLinear (Mat24 a) (Mat32 b) = Mat34 (b %*% a)
>composeLinear (Mat21 a) (Mat42 b) = Mat41 (b %*% a)
>composeLinear (Mat22 a) (Mat42 b) = Mat42 (b %*% a)
>composeLinear (Mat23 a) (Mat42 b) = Mat43 (b %*% a)
>composeLinear (Mat24 a) (Mat42 b) = Mat44 (b %*% a)

>composeLinear (Mat31 a) (Mat13 b) = Mat11 (b %*% a)
>composeLinear (Mat32 a) (Mat13 b) = Mat12 (b %*% a)
>composeLinear (Mat33 a) (Mat13 b) = Mat13 (b %*% a)
>composeLinear (Mat34 a) (Mat13 b) = Mat14 (b %*% a)
>composeLinear (Mat31 a) (Mat23 b) = Mat21 (b %*% a)
>composeLinear (Mat32 a) (Mat23 b) = Mat22 (b %*% a)
>composeLinear (Mat33 a) (Mat23 b) = Mat23 (b %*% a)
>composeLinear (Mat34 a) (Mat23 b) = Mat24 (b %*% a)
>composeLinear (Mat31 a) (Mat33 b) = Mat31 (b %*% a)
>composeLinear (Mat32 a) (Mat33 b) = Mat32 (b %*% a)
>composeLinear (Mat33 a) (Mat33 b) = Mat33 (b %*% a)
>composeLinear (Mat34 a) (Mat33 b) = Mat34 (b %*% a)
>composeLinear (Mat31 a) (Mat43 b) = Mat41 (b %*% a)
>composeLinear (Mat32 a) (Mat43 b) = Mat42 (b %*% a)
>composeLinear (Mat33 a) (Mat43 b) = Mat43 (b %*% a)
>composeLinear (Mat34 a) (Mat43 b) = Mat44 (b %*% a)

>composeLinear (Mat41 a) (Mat14 b) = Mat11 (b %*% a)
>composeLinear (Mat42 a) (Mat14 b) = Mat12 (b %*% a)
>composeLinear (Mat43 a) (Mat14 b) = Mat13 (b %*% a)
>composeLinear (Mat44 a) (Mat14 b) = Mat14 (b %*% a)
>composeLinear (Mat41 a) (Mat24 b) = Mat21 (b %*% a)
>composeLinear (Mat42 a) (Mat24 b) = Mat22 (b %*% a)
>composeLinear (Mat43 a) (Mat24 b) = Mat23 (b %*% a)
>composeLinear (Mat44 a) (Mat24 b) = Mat24 (b %*% a)
>composeLinear (Mat41 a) (Mat34 b) = Mat31 (b %*% a)
>composeLinear (Mat42 a) (Mat34 b) = Mat32 (b %*% a)
>composeLinear (Mat43 a) (Mat34 b) = Mat33 (b %*% a)
>composeLinear (Mat44 a) (Mat34 b) = Mat34 (b %*% a)
>composeLinear (Mat41 a) (Mat44 b) = Mat41 (b %*% a)
>composeLinear (Mat42 a) (Mat44 b) = Mat42 (b %*% a)
>composeLinear (Mat43 a) (Mat44 b) = Mat43 (b %*% a)
>composeLinear (Mat44 a) (Mat44 b) = Mat44 (b %*% a)
>composeLinear (MatInd a) (MatInd b) = MatInd (b %*% a)
>composeLinear (Mat1Ind a) (MatInd1 b) = MatInd (b %*% a)
>composeLinear (Mat2Ind a) (MatInd2 b) = MatInd (b %*% a)
>composeLinear (Mat3Ind a) (MatInd3 b) = MatInd (b %*% a)
>composeLinear (Mat4Ind a) (MatInd4 b) = MatInd (b %*% a)
>composeLinear (MatInd a) (Mat1Ind b) = Mat1Ind (b %*% a)
>composeLinear (MatInd1 a) (Mat1Ind b) = Mat11 (b %*% a)
>composeLinear (MatInd1 a) (Mat2Ind b) = Mat21 (b %*% a)
>composeLinear (MatSS a) (MatSS b) = MatSS (b %*% a)
>composeLinear (MatS1 a) (Mat1S b) = Mat11 (b %*% a)
>composeLinear (MatS1 a) (Mat2S b) = Mat21 (b %*% a)
>composeLinear (MatS1 a) (Mat3S b) = Mat31 (b %*% a)
>composeLinear (MatS1 a) (Mat4S b) = Mat41 (b %*% a)
>composeLinear (MatS2 a) (Mat1S b) = Mat12 (b %*% a)
>composeLinear (MatS2 a) (Mat2S b) = Mat22 (b %*% a)
>composeLinear (MatS2 a) (Mat3S b) = Mat32 (b %*% a)
>composeLinear (MatS2 a) (Mat4S b) = Mat42 (b %*% a)
>composeLinear (MatS3 a) (Mat1S b) = Mat13 (b %*% a)
>composeLinear (MatS3 a) (Mat2S b) = Mat23 (b %*% a)
>composeLinear (MatS3 a) (Mat3S b) = Mat33 (b %*% a)
>composeLinear (MatS3 a) (Mat4S b) = Mat43 (b %*% a)
>composeLinear (MatS4 a) (Mat1S b) = Mat14 (b %*% a)
>composeLinear (MatS4 a) (Mat2S b) = Mat24 (b %*% a)
>composeLinear (MatS4 a) (Mat3S b) = Mat34 (b %*% a)
>composeLinear (MatS4 a) (Mat4S b) = Mat44 (b %*% a)

>instance Category LinearMap where
>  id = MatIdentity
>  (.) = composeLinear

>appLinear :: (LinearTransform f g a) => f a :-> g b -> f a -> g b
>appLinear MatIdentity = id
>appLinear (Mat11 f) = (f <<*>)
>appLinear (Mat12 f) = (f <<*>)
>appLinear (Mat13 f) = (f <<*>)
>appLinear (Mat14 f) = (f <<*>)
>appLinear (Mat21 f) = (f <<*>)
>appLinear (Mat22 f) = (f <<*>)
>appLinear (Mat23 f) = (f <<*>)
>appLinear (Mat24 f) = (f <<*>)
>appLinear (Mat31 f) = (f <<*>)
>appLinear (Mat32 f) = (f <<*>)
>appLinear (Mat33 f) = (f <<*>)
>appLinear (Mat34 f) = (f <<*>)
>appLinear (Mat41 f) = (f <<*>)
>appLinear (Mat42 f) = (f <<*>)
>appLinear (Mat43 f) = (f <<*>)
>appLinear (Mat44 f) = (f <<*>)
>appLinear (MatInd f) = (f <<*>)
>appLinear (MatInd1 f) = (f <<*>)
>appLinear (MatInd2 f) = (f <<*>)
>appLinear (MatInd3 f) = (f <<*>)
>appLinear (MatInd4 f) = (f <<*>)
>appLinear (Mat1Ind f) = (f <<*>)
>appLinear (Mat2Ind f) = (f <<*>)
>appLinear (Mat3Ind f) = (f <<*>)
>appLinear (Mat4Ind f) = (f <<*>)
>appLinear (MatD1 f) = (f <<*>)
>appLinear (MatD2 f) = (f <<*>)
>appLinear (MatD3 f) = (f <<*>)
>appLinear (MatD4 f) = (f <<*>)
>appLinear (MatDD f) = (f <<*>)
>appLinear (MatSS f) = (f <<*>)
>appLinear (MatS1 f) = (f <<*>)
>appLinear (MatS2 f) = (f <<*>)
>appLinear (MatS3 f) = (f <<*>)
>appLinear (MatS4 f) = (f <<*>)
>appLinear (Mat1S f) = (f <<*>)
>appLinear (Mat2S f) = (f <<*>)
>appLinear (Mat3S f) = (f <<*>)
>appLinear (Mat4S f) = (f <<*>)

instance (forall b. VectorSpace (g b), forall b. VectorSpace (f b), Indexable f a, Num a, Scalar (g a) ~ a, Indexable g a, Scalar (f a) ~ Scalar (g a), LinearTransform f g a)
 => VectorSpace (f a :-> g a) where
   type Scalar (f a :-> g a) = a
   vzero = LinearMap Refl $ matrix (\_ _ -> 0) vzero vzero
   vnegate (LinearMap p f) = LinearMap p $ vnegate f
   a %* (LinearMap p f) = LinearMap p $ a %* f
   (LinearMap p f) %+ (LinearMap q g) = LinearMap (vec2_cast p q) $ f %+ g

>instance (Ord a, Fractional a, ConjugateSymmetric a) => Fractional ((Vector2 :*: Vector2) a) where
>   recip = inverseImpl
>   fromRational x = diagonalMatrixImpl (constant2 $ fromRational x) vzero

>instance (ConjugateSymmetric a, Fractional a, Ord a) => LinearInvertible LinearMap Vector2 a
>instance (ConjugateSymmetric a, Fractional a, Ord a) => LinearInvertible LinearMap Vector3 a
>instance (ConjugateSymmetric a, Fractional a, Ord a) => LinearInvertible LinearMap Vector4 a
>instance (ConjugateSymmetric a, Num a, Ord a) => LinearTraceable LinearMap Vector1 a
>instance (ConjugateSymmetric a, Num a, Ord a) => LinearTraceable LinearMap Vector2 a
>instance (ConjugateSymmetric a, Num a, Ord a) => LinearTraceable LinearMap Vector3 a
>instance (ConjugateSymmetric a, Num a, Ord a) => LinearTraceable LinearMap Vector4 a

>linearMatrixMultiply :: (SupportsMatrixMultiplication g h f a,
>    Linearizable LinearMap (:*:) h f a,
>    Linearizable LinearMap (:*:) g h a,
>    Linearizable LinearMap (:*:) g f (Scalar (h a))
> ) => g a :-> h a -> h a :-> f a -> g (Scalar (h a)) :-> f (Scalar (h a))
>linearMatrixMultiply f g = linear (fromLinear f %*% fromLinear g)

>isInvertibleMatrix :: (Eq a, Num a,Traceable m a, LinearTraceable LinearMap m a)
> => (m a :-> m a) -> Bool
>isInvertibleMatrix m = determinant m /= 0

>invertMatrix :: (Eq a, Num a,Invertible m a, LinearInvertible LinearMap m a
> ) => (m a) :-> (m a)
> -> Maybe ((m a) :-> (m a))
>invertMatrix m | isInvertibleMatrix m = Just (inverse m)
>                | otherwise = Nothing

>diagonalMatrix :: (Diagonalizable m a, Linearizable LinearMap (:*:) m m a)
> => m a -> m a :-> m a -> m a :-> m a
>diagonalMatrix v m = linear $ diagonalMatrixImpl v (fromLinear m)

>linearInverseImpl :: (Linearizable LinearMap (:*:) f g a, Linearizable LinearMap (:*:) g f a,
> Transposable f g a
> ) =>  (f :*: g) a -> g a :-> f a
>linearInverseImpl m = linear $ transposeImpl m

>linOuter :: (Linearizable LinearMap (:*:) f g a, Diagonalizable f a,
>      InnerProductSpace (f a), VectorSpace (g a),Num a,
>      Scalar (f a) ~ Scalar (g a))
>  => f a -> g a -> f a :-> g a
>linOuter x y = arrLinear (outer x y)

>matrixLin :: (LinearTransform f g c, Linearizable LinearMap (:*:) f g c)
> => (a -> b -> c) -> f a -> g b -> f c :-> g c
>matrixLin f x y = linear $ matrix f x y

>linmatrix :: (Functor g, Functor f, LinearTransform ((,) (t a)) v (u a),
>  Linearizable LinearMap (:*:) f g (v (u a)))
> => ((t a,u a) :-> v (u a)) -> (f (t a), g (u a)) -> f (v (u a)) :-> g (v (u a))
>linmatrix f (ma, nb) = linear $ Matrix $
>   flip fmap ma $ \a ->
>   flip fmap nb $ \b -> f -!< (a, b)

>arrmatrix :: (FunctorArrow f arr arr, FunctorArrow g arr arr, ArrowApply arr)
>          => arr (a,b) c -> arr (f a, g b) ((f :*: g) c)
>arrmatrix f = proc (ma, nb) -> do
>    res <- amap (proc a -> amap (proc b -> f -< (a,b)) -<< nb) -<< ma
>    returnA -< Matrix res

>-- | <https://en.wikipedia.org/wiki/Bilinear_map>
>bilinear :: (VectorSpace ((f :*: g) a), VectorSpace (g a), VectorSpace (f a), a ~ Scalar (g a), Indexable f a, Indexable g a, Integral a,
>  Linearizable LinearMap (:*:) f g a)
>   => (f a -> f a -> g a) -> f a -> f a -> f a :-> g a
>bilinear f a b = linear (bilinearImpl f a b)

>dual_map :: (Transposable g f a, Linearizable arr1 (:*:) f g a, Linearizable arr2 (:*:) g f a)
> => arr2 (g a) (f a) -> arr1 (f a) (g a)
>dual_map f = linear $ transposeImpl $ fromLinear f

dual_map :: (LinearTransform f I a, Diagonalizable f Integer, a ~ Scalar (f a), a ~ Scalar (g a), VectorSpace (g a), VectorSpace (f a), Diagonalizable f a, LinearTransform f g a)
  => Dual (f a) -> g a -> LinearMap (f a) (g a)
dual_map (Covector (lm :: f a :-> Vector1 a)) v = arrLinear (\u -> (unI $ lm -!< u) %* v) (vector_dimension $ (indexable_indices :: f Integer))

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
>   matIdentity = Mat11 identity
>instance HasIdentityLinear Vector2 LinearMap where
>   matIdentity = Mat22 identity
>instance HasIdentityLinear Vector3 LinearMap where
>   matIdentity = Mat33 identity
>instance HasIdentityLinear Vector4 LinearMap where
>   matIdentity = Mat44 identity

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

>fromLinearImpl :: (Diagonalizable f a, Num a) => f a :-> g a -> (f :*: g) a
>fromLinearImpl MatIdentity = identity
>fromLinearImpl (Mat11 x) = x
>fromLinearImpl (Mat12 x) = x
>fromLinearImpl (Mat13 x) = x
>fromLinearImpl (Mat14 x) = x
>fromLinearImpl (Mat21 x) = x
>fromLinearImpl (Mat22 x) = x
>fromLinearImpl (Mat23 x) = x
>fromLinearImpl (Mat24 x) = x
>fromLinearImpl (Mat31 x) = x
>fromLinearImpl (Mat32 x) = x
>fromLinearImpl (Mat33 x) = x
>fromLinearImpl (Mat34 x) = x
>fromLinearImpl (Mat41 x) = x
>fromLinearImpl (Mat42 x) = x
>fromLinearImpl (Mat43 x) = x
>fromLinearImpl (Mat44 x) = x
>fromLinearImpl (MatInd x) = x
>fromLinearImpl (MatInd1 x) = x
>fromLinearImpl (MatInd2 x) = x
>fromLinearImpl (MatInd3 x) = x
>fromLinearImpl (MatInd4 x) = x
>fromLinearImpl (Mat1Ind x) = x
>fromLinearImpl (Mat2Ind x) = x
>fromLinearImpl (Mat3Ind x) = x
>fromLinearImpl (Mat1D x) = x
>fromLinearImpl (Mat2D x) = x
>fromLinearImpl (Mat3D x) = x
>fromLinearImpl (Mat4D x) = x
>fromLinearImpl (MatD1 x) = x
>fromLinearImpl (MatD2 x) = x
>fromLinearImpl (MatD3 x) = x
>fromLinearImpl (MatD4 x) = x
>fromLinearImpl (MatSS x) = x
>fromLinearImpl (MatS1 x) = x
>fromLinearImpl (MatS2 x) = x
>fromLinearImpl (MatS3 x) = x
>fromLinearImpl (MatS4 x) = x
>fromLinearImpl (Mat1S x) = x
>fromLinearImpl (Mat2S x) = x
>fromLinearImpl (Mat3S x) = x
>fromLinearImpl (Mat4S x) = x


>linear_apply :: (LinearTransform m n a, Linearizable arr (:*:) m n a)
> => arr (m a) (n a) -> m a -> n a
>linear_apply f = (fromLinear f <<*>)

>linear_apply_inverse :: (LinearTransform m n a, Linearizable arr (:*:) m n a)
> => arr (m a) (n a) -> n a -> m a
>linear_apply_inverse f = (<*>> fromLinear f)

>-- | for linear maps that produce scalars
>(-!!<) :: (LinearTransform f Vector1 r) => f r :-> Vector1 r -> f r -> r
>f -!!< x = vectorElement (f -!< x)

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
linear_identity_indexable = linear $ identityImpl indexable_indices

>matrix_equalizer :: (Eq (g (f a)),
>   Linearizable LinearMap (:*:) h f a,
>   Linearizable LinearMap (:*:) g h a,
>  SupportsMatrixMultiplication g h f a,
>                     Applicative g, Applicative f,
>                     Foldable g, Foldable f  
>               )
>                 => (g a :-> h a) -> (g a :-> h a) -> Prop (h a :-> f a)
>matrix_equalizer a b = Characteristic $ \v -> let
>     a' = fromLinear a
>     b' = fromLinear b
>     v' = fromLinear v
>   in a' %*% v' == b' %*% v'

>cov1 :: (ConjugateSymmetric a, Num a) => Vector1 (Dual (Vector1 a))
>cov1 = Vector1 (covector $ vectorElement)

>instance (ConjugateSymmetric a, Num a) => ProjectionDual Vector1 Dual a where
>   projectionDual = cov1

instance (Num a, a ~ Scalar a) => FiniteDimensional (Vector1 a) Dual I LinearMap where
   finite = LinearMap Refl $ \ (Matrix (Covector f)) ->
               (f -!< (covector $ vectorElement))

>instance (ConjugateSymmetric a,Num a) => Dualizable (Vector1 a) Dual where
>   covector = covectorImpl
>   bracket = bracketImpl

>instance (ConjugateSymmetric a, Num a, Closed a) => VectorDerivative (Vector1 a) Dual LinearMap where
>   divergence f = partialDerivate1x (covector (vectorElement . (-!<) f))
>   grad f = arrLinear $ \z -> Vector1 (partialDerivate1x f `bracket` z)
>   directionalDerivative = directionalDerivativeImpl


>-- | <https://en.wikipedia.org/wiki/Directional_derivative>
>-- Note: the direction v is not guaranteed to be normalized.
>directionalDerivativePrim :: (Infinitesimal str (Scalar (v a)),
> Fractional (Scalar (v a)), VectorSpace (v a))
>  => v a -> (v a -> Scalar (v a)) -> v a -> Closure str (Scalar (v a))
>directionalDerivativePrim v f x = let
>  fx = f x
> in limit $ do
>   h <- epsilonStream
>   return $ (1/h) * (f (x %+ h %* v) - fx)
>
>data Tangent str p where
> Tangent :: { tangentBasePoint :: t, tangentVector :: Closure str a }
>         -> Tangent str (t,a)

>vectorDirectionalDerivativePrim ::
>    (Infinitesimal str (Scalar v), VectorSpace a, VectorSpace v,
>     Limiting str a, Scalar a ~ Scalar v)
> =>  v -> (v -> a) -> v -> Tangent str (v, a)
>vectorDirectionalDerivativePrim v f x = Tangent {
>  tangentBasePoint = x,
>  tangentVector = limit $ do
>               h <- epsilonStream
>               return $ (1/h) %* (f (x %+ h %* v) %- fx) }
> where fx = f x

>-- | <https://en.wikipedia.org/wiki/Directional_derivative>
>-- Note: the direction v is not guaranteed to be normalized.
>directionalDerivativeImpl :: (LinearTransform v Vector1 a, Closed a, VectorSpace (v a), Linearizable LinearMap (:*:) v Vector1 a, Diagonalizable v a, Scalar (v a) ~ a)
>  => v a -> Dual (v a) -> Dual (v a)
>directionalDerivativeImpl v (Covector f) = Covector $ arrLinear $
>   Vector1 . accumulationPoint
>           . directionalDerivativePrim v (vectorElement . appLinear f)

>vectorDirectionalDerivative ::
>  (Linearizable LinearMap (:*:) f g a, LinearTransform f g a, 
>   Diagonalizable f a, Closed (g a), Num a,
>   VectorSpace (g a), VectorSpace (f a),
>   Infinitesimal Stream (Scalar (f a)))
>   => f a -> f a :-> g a -> f a :-> g a
>vectorDirectionalDerivative v g = arrLinear $ 
>   accumulationPoint . tangentVector .
>   vectorDirectionalDerivativePrim v (appLinear g) 

>directionalDerivativeImpl_ ::
>  (a ~ Scalar (v a), LinearTransform v v a, InnerProductSpace (v a),
>   Linearizable LinearMap (:*:) v Vector1 a, Diagonalizable v a,
>   Dualizable (v a) d,
>   VectorDerivative (v a) d LinearMap)
>  => v a -> d (v a) -> d (v a)
>directionalDerivativeImpl_ v d = covector $ \x -> v %. (grad d -!< x)


-- | <https://en.wikipedia.org/wiki/Conjugate_transpose>
instance (ConjugateSymmetric a) => ConjugateSymmetric (Vector1 a :-> Vector1 a) where
   conj = amap conj . transpose

-- | <https://en.wikipedia.org/wiki/Dual_space>
instance StandardBasis ((Dual :*: Vector1) a) where
  unitVectors = [Matrix (Covector $ linear $ Matrix $ Covector $ id_linear Proxy)]

>-- | see "Lawvere,Rosebrugh: Sets for mathematics", pg. 167.
>instance (SupportsMatrixMultiplication Vector1 Vector1 Vector1 a)
> => Monoid (Vector1 a :-> Vector1 a) where
>   mempty = linear identity
>   mappend a b = linear $ fromLinear a %*% fromLinear b

>instance (SupportsMatrixMultiplication Vector1 Vector1 Vector1 a)
> => Semigroup (Vector1 a :-> Vector1 a) where
>   a <> b = linear $ fromLinear a %*% fromLinear b

>partialDerivate1x :: (Closed a, Num a, ConjugateSymmetric a)
>              => Dual (Vector1 a) -> Dual (Vector1 a)
>partialDerivate1x (Covector f) = covector $ partialDerivate ch (vectorElement . appLinear f)
>   where ch eps (Vector1 x) = Vector1 (x+eps)

>cov2 :: (ConjugateSymmetric a, Num a) => Vector2 (Dual (Vector2 a))
>cov2 = Vector2 (covector xcoord2) (covector ycoord2)

>instance (ConjugateSymmetric a, Num a) => ProjectionDual Vector2 Dual a where
>   projectionDual = cov2

instance (Num a) => FiniteDimensional (Vector2 a) Dual Vector1 LinearMap where
   finite = arrLinear $ \ (Matrix (Covector f)) -> let f' = appLinear f
             in Vector1 $ Vector2 (f' (covector xcoord2)) (f' (covector ycoord2))

>instance (Num a,ConjugateSymmetric a) => Dualizable (Vector2 a) Dual where
>   covector = covectorImpl
>   bracket = bracketImpl

>instance (ConjugateSymmetric a, Infinitesimal Stream a, Closed a) => VectorDerivative (Vector2 a) Dual LinearMap where
>   divergence = divergence2
>   grad = grad2
>   directionalDerivative = directionalDerivativeImpl

>divergence2 :: (ConjugateSymmetric a, Closed a, Fractional a) => LinearMap (Vector2 a) (Vector2 a) -> Dual (Vector2 a)
>divergence2 f = partialDerivate2x (linearDual_2x f)
>             %+ partialDerivate2y (linearDual_2y f)

>linearDual_2x :: (Dualizable (f v) d, Diagonalizable f v, LinearTransform f Vector2 v, Linearizable LinearMap (:*:) f Vector1 v)
> => LinearMap (f v) (Vector2 v) -> d (f v)
>linearDual_2x f = covector (xcoord2 . (-!<) f)
>
>linearDual_2y :: (Dualizable (f v) d, Diagonalizable f v, LinearTransform f Vector2 v, Linearizable LinearMap (:*:) f Vector1 v)
> => LinearMap (f v) (Vector2 v) -> d (f v)
>linearDual_2y f = covector (ycoord2 . (-!<) f)

>grad2 :: (ConjugateSymmetric a, Closed a, Fractional a) => Dual (Vector2 a) -> LinearMap (Vector2 a) (Vector2 a)
>grad2 f = arrLinear $ \z -> Vector2 (partialDerivate2x f `bracket` z)
>                                    (partialDerivate2y f `bracket` z)

>curl2 :: (ConjugateSymmetric a, VectorSpace a,Fractional a, Closed a) => LinearMap (Vector2 a) (Vector2 a) -> LinearMap (Vector2 a) (Vector2 a)
>curl2 f = arrLinear $ \z -> Vector2 (partialDerivate2y fx `bracket` z
>                     %- partialDerivate2x fy `bracket` z)
>                    (partialDerivate2x fy `bracket` z 
>                     %- partialDerivate2y fx `bracket` z)
>  where fx = linearDual_2x f
>        fy = linearDual_2y f

>instance (ConjugateSymmetric a, Infinitesimal Stream a, VectorSpace a,Closed a) => VectorCrossProduct (Vector2 a) LinearMap where
>  curl = curl2
>
>instance (Infinitesimal Stream a, Closed a) => VectorLaplacian (Vector2 a) LinearMap

>cov3 :: (Num a, Ord a, ConjugateSymmetric a) => Vector3 (Dual (Vector3 a))
>cov3 = Vector3 (covector xcoord3) (covector ycoord3) (covector zcoord3)

>instance (Num a, Ord a, ConjugateSymmetric a) => ProjectionDual Vector3 Dual a where
>   projectionDual = cov3

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
>   covector = covectorImpl
>   bracket = bracketImpl

>instance (ConjugateSymmetric a,Num a) => Dualizable (Vector4 a) Dual where
>   covector = covectorImpl
>   bracket = bracketImpl

>instance (Closed a, Num a, ConjugateSymmetric a) => VectorDerivative (Vector3 a) Dual LinearMap where
>  divergence = divergence3
>  grad = grad3
>  directionalDerivative = directionalDerivativeImpl

>instance (Ord a, Closed a, Num a, ConjugateSymmetric a) => VectorCrossProduct (Vector3 a) LinearMap where
>  curl = curl3

>instance (Ord a, Closed a, Num a, ConjugateSymmetric a) => VectorLaplacian (Vector3 a) LinearMap where
>  vectorLaplace = vectorLaplace3

>defaultVectorLaplace :: (VectorDerivative v Dual arr, VectorCrossProduct v arr,
>                  VectorSpace (arr v v)) => arr v v -> arr v v
>defaultVectorLaplace a = grad (divergence a) %- curl (curl a)

>vectorLaplace3 :: (a ~ Scalar (f a), VectorDerivative (f a) Dual LinearMap,
> LinearTransform f Vector3 a,
> LinearTransform f Vector1 a,
> Diagonalizable f a, Dualizable (f a) Dual,
> Linearizable LinearMap (:*:) f Vector3 a,
> Linearizable LinearMap (:*:) f Vector1 a
> ) =>
>  f a :-> (Vector3 a) -> f a :-> (Vector3 a)
>vectorLaplace3 f = arrLinear $ \x -> Vector3
> ((laplace $ linearDual_3x f) `bracket` x)
> ((laplace $ linearDual_3y f) `bracket` x)
> ((laplace $ linearDual_3z f) `bracket` x)

>-- | <https://en.wikipedia.org/wiki/Divergence>
>divergence3 :: (Closed a, ConjugateSymmetric a, Num a)
> => (Vector3 a) :-> (Vector3 a) -> Dual (Vector3 a)
>divergence3 f = partialDerivate3x (linearDual_3x f)
>              %+ partialDerivate3y (linearDual_3y f)
>              %+ partialDerivate3z (linearDual_3z f)

>-- | <https://en.wikipedia.org/wiki/Gradient>
>grad3 :: (Num s,ConjugateSymmetric s,Closed s) => Dual (Vector3 s) -> (Vector3 s) :-> (Vector3 s)
>grad3 f = arrLinear $ \z -> Vector3 (partialDerivate3x f `bracket` z)
>                                    (partialDerivate3y f `bracket` z)
>                                    (partialDerivate3z f `bracket` z)

>-- | <https://en.wikipedia.org/wiki/Laplace_operator>
>laplace3 :: (Num a, ConjugateSymmetric a,Closed a) => Dual (Vector3 a) -> Dual (Vector3 a)
>laplace3 f = divergence3 (grad3 f)


>-- | <https://en.wikipedia.org/wiki/Curl_(mathematics)>

>curl3 :: (ConjugateSymmetric a,Num a, Ord a, Closed a) => (Vector3 a) :-> (Vector3 a)  -> (Vector3 a) :-> (Vector3 a)
>curl3 f = arrLinear $ \z -> Vector3 ((partialDerivate3y fz %- partialDerivate3z fy)
>                       `bracket` z)
>                    ((partialDerivate3z fx %- partialDerivate3x fz)
>                       `bracket` z)
>                    ((partialDerivate3x fy %- partialDerivate3y fx)
>                       `bracket` z)
>  where fx = linearDual_3x f
>        fy = linearDual_3y f
>        fz = linearDual_3z f

>linearDual_3x :: (Dualizable (f a) Dual, Diagonalizable f a,Linearizable LinearMap (:*:) f Vector1 a,LinearTransform f Vector3 a) => (f a) :-> (Vector3 a) -> Dual (f a)
>linearDual_3x f = covector (xcoord3 . (-!<) f)

>linearDual_3y :: (Dualizable (f a) Dual, Diagonalizable f a,Linearizable LinearMap (:*:) f Vector1 a,LinearTransform f Vector3 a) => (f a) :-> (Vector3 a) -> Dual (f a)
>linearDual_3y f = covector (ycoord3 . (-!<) f)

>linearDual_3z :: (Linearizable LinearMap (:*:) f Vector1 a,
>                  Diagonalizable f a, Dualizable (f a) Dual,
>                  LinearTransform f Vector3 a)
> => (f a) :-> (Vector3 a) -> Dual (f a)
>linearDual_3z f = covector (zcoord3 . (-!<) f)

>partialDerivate2x :: (Fractional a,Closed a, ConjugateSymmetric a) 
>                   => Dual (Vector2 a) -> Dual (Vector2 a)
>partialDerivate2x (Covector f) = covector $ partialDerivate ch (vectorElement . appLinear f)
>  where ch eps (Vector2 x y) = Vector2 (x+eps) y

>partialDerivate2y :: (Fractional a, Closed a, ConjugateSymmetric a)
>                   => Dual (Vector2 a) -> Dual (Vector2 a)
>partialDerivate2y (Covector f) = covector $ partialDerivate ch (vectorElement . appLinear f)
>  where ch eps (Vector2 x y) = Vector2 x (y+eps)


>partialDerivate3x :: (ConjugateSymmetric a, Num a,Closed a)
>                   => Dual (Vector3 a) -> Dual (Vector3 a)
>partialDerivate3x (Covector f) = covector $ partialDerivate dx3 (vectorElement . appLinear f)

>partialDerivate3y :: (ConjugateSymmetric a, Num a,Closed a) => Dual (Vector3 a) -> Dual (Vector3 a)
>partialDerivate3y (Covector f) = covector $ partialDerivate dy3 (vectorElement . appLinear f)
          
>partialDerivate3z :: (ConjugateSymmetric a,Closed a, Num a) => Dual (Vector3 a) -> Dual (Vector3 a)
>partialDerivate3z (Covector f) = covector $ partialDerivate dz3 (vectorElement . appLinear f)

>-- | \[\nabla_3\], three-dimensional partial derivate. Use Applicative.\<*\> for applying it.
>del3 :: (ConjugateSymmetric v,Num v,Closed v) => Vector3 (Dual (Vector3 v) -> Dual (Vector3 v))
>del3 = Vector3 partialDerivate3x partialDerivate3y partialDerivate3z


>-- outer product \[\nabla_3 \otimes \nabla_3\] operator for three-dimensional vectors.
>del3Squared :: (Num v, Closed v, ConjugateSymmetric v)
>  => (Vector3 :*: Vector3) (Dual (Vector3 v) -> Dual (Vector3 v))
>del3Squared = matrix (.) del3 del3

>hessian3 :: (Closed v, ConjugateSymmetric v, Num v)
> => Dual (Vector3 v) -> (Vector3 :*: Vector3) (Dual (Vector3 v))
>hessian3 f = matrix (\a b -> a (b f)) del3 del3

>-- | <https://en.wikipedia.org/wiki/Dual_space>
>instance (Num a, ConjugateSymmetric a) => StandardBasis ((Dual :*: Vector3) a) where
>  unitVectors = [dualUnitvector3 i3,
>                  dualUnitvector3 j3,
>                  dualUnitvector3 k3]

>dualUnitvector3 :: (Num a, ConjugateSymmetric a) => Vector3 a -> (Dual :*: Vector3) a
>dualUnitvector3 v = Matrix (Covector $ Mat31 $ Matrix $ fmap Vector1 v)

>-- | this computes partial derivates of the scalar-valued 3D vector field
>-- along each variable simultaneously.
>-- \[\nabla f({\mathbb{v}}) = \frac{\partial \mathbb{f}}{\partial x}{\mathbb{i}}
>--                     + \frac{\partial \mathbb{f}}{\partial y}{\mathbb{j}}
>--                     + \frac{\partial \mathbb{f}}{\partial z}{\mathbb{k}}\]
>delVector3 :: (Infinitesimal s a)
>  => (Vector3 a -> a) -> Vector3 a -> Vector3 (Closure s a)
>delVector3 f (Vector3 x y z) = Vector3 (partialDerivate1_3 ff x y z)
>                                        (partialDerivate2_3 ff x y z)
>                                        (partialDerivate3_3 ff x y z)
>  where ff a b c = f (Vector3 a b c)

>instance DifferentialOperator Vector3 where
>   partial = delPartial3

>delPartial3 :: (DifferentiallyClosed a) => (Vector3 a -> a) -> Vector3 a -> Vector3 a
>delPartial3 f (Vector3 x y z) = Vector3 (partial1_3 ff x y z)
>                                         (partial2_3 ff x y z)
>                                         (partial3_3 ff x y z)
>  where ff a b c = f (Vector3 a b c)

>partialDerivateVector3 :: (Infinitesimal Stream (Scalar a)
> , VectorSpace a, Num a, Limiting Stream a)
>   => (Vector3 (Scalar a) -> a) -> Vector3 (Scalar a) -> Closure Stream (Vector3 a)
>partialDerivateVector3 f (Vector3 x y z) = Vector3Closure $
>      Vector3 (pd1 (callf f) x y z)
>              (pd2 (callf f) x y z)
>              (pd3 (callf f) x y z)
>   where
>      callf ff a b c = ff (Vector3 a b c)
>      pd1 ff a b c = limit $ epsilonStream >>= \ eps ->
>            return $ (1/(2*eps)) %* (ff (a+eps) b c - ff (a-eps) b c)
>      pd2 ff a b c = limit $ epsilonStream >>= \ eps ->
>            return $ (1/(2*eps)) %* (ff a (b+eps) c - ff a (b-eps) c)
>      pd3 ff a b c = limit $ epsilonStream >>= \ eps ->
>            return $ (1/(2*eps)) %* (ff a b (c+eps) - ff a b (c-eps))


>-- | partial derivate a function defined for each coordinate along
>-- each dimension of three-dimensional vector.
>pderive3 :: (Closed a, Num a)
>   => Vector3 (a -> a) -> Vector3 a -> Vector3 a
>pderive3 (Vector3 fx fy fz) (Vector3 x y z) = Vector3
>   (partialDerive (+) fx x)
>   (partialDerive (+) fy y)
>   (partialDerive (+) fz z)

>vectorFieldDerivate :: (Closed a, Num a)
>  => (Vector3 a -> a) -> Vector3 a -> Vector3 a
>vectorFieldDerivate f v@(Vector3 x y z) = Vector3
>   (partialDerivate dx3 f v)
>   (partialDerivate dy3 f v)
>   (partialDerivate dz3 f v)

>matBind :: (Monad f) => (f :*: g) a -> (g a -> (f :*: h) b) -> (f :*: h) b
>matBind (Matrix m) f = Matrix $ m >>= (cells . f)

>cellsLinear :: (Linearizable LinearMap (:*:) f g a) => (f a :-> g a) -> f (g a)
>cellsLinear = cells . fromLinear

>linBind :: (Linearizable LinearMap (:*:) f g a,
>            Linearizable LinearMap (:*:) f h b, Monad f)
>  => f a :-> g a -> (g a -> f b :-> h b) -> f b :-> h b
>linBind m f = linear $ matBind (fromLinear m) (fromLinear . f)

>covectorImpl :: (Linearizable LinearMap (:*:) f Vector1 b,
> Scalar (f b) ~ b, Diagonalizable f b, Num b)
> => (f b -> b) -> Dual (f b)
>covectorImpl f = Covector $ arrLinear $ Vector1 . f

>linearOuterProduct_ a b = arrLinear (Math.Matrix.Interface.outer a b)

>arrLinear :: (Linearizable arr (:*:) f g a, Diagonalizable f a, Num a)
>  => (f a -> g a) -> arr (f a) (g a)
>arrLinear f = linear $ functionMatrix f
>
>arrNatural :: (Linearizable arr (:*:) f g a, Diagonalizable f a, Num a) => f :~> g -> arr (f a) (g a)
>arrNatural (NatTrans m) = arrLinear m

>bracketMap :: Dual (f a) -> f a :-> Vector1 (Scalar (f a))
>bracketMap (Covector f) = f

>bracketImpl :: (LinearTransform f Vector1 a) => Dual (f a) -> f a -> a
>bracketImpl x = vectorElement . appLinear (bracketMap x)
