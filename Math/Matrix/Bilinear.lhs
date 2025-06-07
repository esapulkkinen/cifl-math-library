>-- -*- coding: utf-8 -*-
>{-# LANGUAGE TypeOperators #-}
>{-# LANGUAGE GADTs #-}
>{-# LANGUAGE ConstraintKinds #-}
>{-# LANGUAGE FlexibleContexts #-}
>{-# LANGUAGE FlexibleInstances #-}
>{-# LANGUAGE TypeFamilies #-}
>{-# LANGUAGE UndecidableInstances #-}

>module Math.Matrix.Bilinear where
>import Math.Matrix.Interface
>import Math.Matrix.Linear

>data BilinearMap v w where
>  BilinearMap :: (v1 a -> v2 a :-> w a) -> (v2 a -> v1 a :-> w a)
>              -> BilinearMap (v1 a,v2 a) (w a)

>data Tensor a b where
>  Tensor :: (f :*: g) (a,b) -> Tensor (f a) (g b)

>type v :=> w = BilinearMap v w


bilinear :: (w a,v a) :=> r a -> (w a ⮾ v a) :-> (w a :-> v a)
bilinear (BilinearMap f g) = arr_linear $ \ (Tensor m) ->
    m <!> (f, g)

>type Bilinear f g h a = (Diagonalizable h a, Applicative f,Applicative g,
> VectorSpace (h a), VectorSpace (g a), VectorSpace (f a),
> Linearizable LinearMap (:*:) g h a,
> Linearizable LinearMap (:*:) h h a,
> Linearizable LinearMap (:*:) f h a)
>
>instance (Bilinear f g h a, Num a) => VectorSpace (BilinearMap (f a, g a) (h a)) where
>  type Scalar (BilinearMap (f a, g a) (h a)) = a
>  vzero = BilinearMap (const vzero) (const vzero)
>  vnegate (BilinearMap f g) = BilinearMap (vnegate . f) (vnegate . g)
>  (BilinearMap f g) %+ (BilinearMap f' g') = BilinearMap (\a -> f a %+ f' a)
>                                                        (\a -> g a %+ g' a)
>  k %* (BilinearMap f g) = BilinearMap (\a -> k %* f a) (\a -> k %* g a)

