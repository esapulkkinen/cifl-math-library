>{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, GADTs,ConstraintKinds #-}
>{-# LANGUAGE TypeFamilies, RankNTypes, UndecidableInstances #-}
>{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
>{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
>{-# LANGUAGE StandaloneDeriving #-}
>module Math.Matrix.Tensor where
>import Math.Matrix.FiniteVector
>import Text.PrettyPrint
>import Math.Tools.PrettyP
>import Math.Tools.NaturalTransformation
>import Data.Kind
>import GHC.TypeLits
>import Data.Proxy
>import Data.List (intersperse)
>import Math.Matrix.Interface
>import Math.Matrix.Vector1
>import Math.Matrix.Vector2
>import Math.Matrix.Vector3
>import Math.Matrix.Vector4

>type (n :: [Nat]) %->% a = Tensor n a

>data family Tensor (n :: [Nat]) a :: Type
>data instance Tensor '[] a = Tensor0 { unTensor0 :: a }
>data instance Tensor '[n] a = Tensor1 { unTensor1 :: Vec n (Tensor '[] a) }
>data instance Tensor '[n,m] a = Tensor2 { unTensor2 :: Vec n (Tensor '[m] a) }
>data instance Tensor '[n,m,k] a = Tensor3 { unTensor3 :: Vec n (Tensor '[m,k] a) }
>data instance Tensor '[n,m,k,l] a = Tensor4 { unTensor4 :: Vec n (Tensor '[m,k,l] a) }
>data instance Tensor '[n,m,k,l,o] a = Tensor5 { unTensor5 :: Vec n (Tensor '[m,k,l,o] a) }
>data instance Tensor '[n,m,k,l,o,p] a = Tensor6 { unTensor6 :: Vec n (Tensor '[m,k,l,o,p] a) }
>data instance Tensor '[n,m,k,l,o,p,q] a = Tensor7 { unTensor7 :: Vec n (Tensor '[m,k,l,o,p,q] a) }
>data instance Tensor '[n,m,k,l,o,p,q,r] a = Tensor8 { unTensor8 :: Vec n (Tensor '[m,k,l,o,p,q,r] a) }

>instance (Show a) => Show (Tensor '[] a) where
>  show (Tensor0 x) = show x

>instance (Show a) => Show (Tensor '[n] a) where
>  show (Tensor1 v) = "{" ++ show v ++ "}"

>instance (Show a) => Show (Tensor '[n,m] a) where
>  show (Tensor2 v) = concat $ intersperse "\n" $ toList (fmap show v)

>instance (PpShow a) => PpShow (Tensor '[] a) where
>  pp (Tensor0 x) = pp x
>instance (PpShow a) => PpShow (Tensor '[n] a) where
>  pp (Tensor1 v) = fsep $ toList $ fmap pp v
>instance (PpShow a) => PpShow (Tensor '[n,m] a) where
>  pp (Tensor2 v) = vcat $ toList $ fmap pp v
>instance (PpShow a) => PpShow (Tensor '[n,m,o] a) where
>  pp (Tensor3 v) = cat $ punctuate (pp '|') $ toList $ fmap pp v
>instance (PpShow a) => PpShow (Tensor '[n,m,o,p] a) where
>  pp (Tensor4 v) = vcat $ punctuate (pp '-') $ toList $ fmap pp v

>deriving instance (Show a) => Show (Tensor '[n,m,k] a)
>deriving instance (Show a) => Show (Tensor '[n,m,k,l] a)
>deriving instance (Show a) => Show (Tensor '[n,m,k,l,o] a)
>deriving instance (Show a) => Show (Tensor '[n,m,k,l,o,p] a)
>deriving instance (Show a) => Show (Tensor '[n,m,k,l,o,p,q] a)
>deriving instance (Show a) => Show (Tensor '[n,m,k,l,o,p,q,r] a)

>class (Functor t) => Tensorable t (n :: [Nat]) | t -> n where
>  tensor :: t a -> Tensor n a

>vector1t :: Vector1 a -> Tensor '[1] a
>vector1t (Vector1 x) = Tensor1 $ Cons (Tensor0 x) Empty
>vector2t :: Vector2 a -> Tensor '[2] a
>vector2t (Vector2 x y) = Tensor1 $ Cons (Tensor0 x) $ Cons (Tensor0 y) $ Empty
>vector3t :: Vector3 a -> Tensor '[3] a
>vector3t (Vector3 x y z) = Tensor1 $ Cons (Tensor0 x) $ Cons (Tensor0 y) $ Cons (Tensor0 z) $ Empty
>vector4t :: Vector4 a -> Tensor '[4] a
>vector4t (Vector4 t x y z) = Tensor1 $ Cons (Tensor0 t) $ Cons (Tensor0 x) $ Cons (Tensor0 y) $ Cons (Tensor0 z) $ Empty

>finitevec1 :: Vector1 a -> Vec 1 a
>finitevec1 (Vector1 x) = Cons x Empty
>finitevec2 :: Vector2 a -> Vec 2 a
>finitevec2 (Vector2 x y) = Cons x $ Cons y $ Empty
>finitevec3 :: Vector3 a -> Vec 3 a
>finitevec3 (Vector3 x y z) = Cons x $ Cons y $ Cons z $ Empty
>finitevec4 :: Vector4 a -> Vec 4 a
>finitevec4 (Vector4 t x y z) = Cons t $ Cons x $ Cons y $ Cons z $ Empty

>matrixt :: (Functor t) => (forall b. t b -> Vec k b) -> (forall b. u b -> Vec k' b) -> (t :*: u) a -> Tensor '[k,k'] a
>matrixt f g (Matrix m) = Tensor2 $ f $ fmap (Tensor1 . fmap Tensor0 . g) m

>matrixt3 :: (Functor t, Functor u) => (forall b. t b -> Vec k b)
>                       -> (forall b. u b -> Vec k' b)
>                       -> (forall b. v b -> Vec k'' b)
>                       -> (t :*: (u :*: v)) a
>                       -> Tensor '[k,k',k''] a
>matrixt3 f g h (Matrix m) = Tensor3 $ f $ fmap (matrixt g h) m

matrixt3' :: (Tensorable t '[n1,m1], Functor f)
 => (f b -> Vec k (Tensor '[m2,k] a))
 -> (Vec n1 (Tensor '[m1] a1) -> b)
 -> (f :*: t) a1
 -> Tensor '[k,m2,k] a

>matrixtt :: (Tensorable t k, Tensorable u m, Functor f)
> => (f :~> t) -> (g :~> u) -> (f :*: g) a -> (Tensor k :*: Tensor m) a
>matrixtt (NatTrans f) (NatTrans g) (Matrix m) = Matrix $
>   tensor $ f $ fmap (tensor . g) m

>instance Tensorable Vector1 '[1] where { tensor = vector1t }
>instance Tensorable Vector2 '[2] where { tensor = vector2t }
>instance Tensorable Vector3 '[3] where { tensor = vector3t }
>instance Tensorable Vector4 '[4] where { tensor = vector4t }

>instance Tensorable (Vector1 :*: Vector1) '[1,1] where { tensor = matrixt finitevec1 finitevec1 }
>instance Tensorable (Vector1 :*: Vector2) '[1,2] where { tensor = matrixt finitevec1 finitevec2 }
>instance Tensorable (Vector1 :*: Vector3) '[1,3] where { tensor = matrixt finitevec1 finitevec3 }
>instance Tensorable (Vector1 :*: Vector4) '[1,4] where { tensor = matrixt finitevec1 finitevec4 }

>instance Tensorable (Vector2 :*: Vector1) '[2,1] where { tensor = matrixt finitevec2 finitevec1 }
>instance Tensorable (Vector2 :*: Vector2) '[2,2] where { tensor = matrixt finitevec2 finitevec2 }
>instance Tensorable (Vector2 :*: Vector3) '[2,3] where { tensor = matrixt finitevec2 finitevec3 }
>instance Tensorable (Vector2 :*: Vector4) '[2,4] where { tensor = matrixt finitevec2 finitevec4 }

>instance Tensorable (Vector3 :*: Vector1) '[3,1] where { tensor = matrixt finitevec3 finitevec1 }
>instance Tensorable (Vector3 :*: Vector2) '[3,2] where { tensor = matrixt finitevec3 finitevec2 }
>instance Tensorable (Vector3 :*: Vector3) '[3,3] where { tensor = matrixt finitevec3 finitevec3 }
>instance Tensorable (Vector3 :*: Vector4) '[3,4] where { tensor = matrixt finitevec3 finitevec4 }

>instance Tensorable (Vector4 :*: Vector1) '[4,1] where { tensor = matrixt finitevec4 finitevec1 }
>instance Tensorable (Vector4 :*: Vector2) '[4,2] where { tensor = matrixt finitevec4 finitevec2 }
>instance Tensorable (Vector4 :*: Vector3) '[4,3] where { tensor = matrixt finitevec4 finitevec3 }
>instance Tensorable (Vector4 :*: Vector4) '[4,4] where { tensor = matrixt finitevec4 finitevec4 }
