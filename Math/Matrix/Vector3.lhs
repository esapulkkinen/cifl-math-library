>{-# LANGUAGE Safe,MultiParamTypeClasses, TypeSynonymInstances #-}
>{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeOperators #-}
>{-# LANGUAGE TypeFamilies, PatternGuards, DataKinds, LambdaCase #-}
>{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveDataTypeable #-}
>{-# LANGUAGE PatternSynonyms #-}
>module Math.Matrix.Vector3 where
>import safe Text.PrettyPrint (sep,vcat,nest, Doc)
>import safe Control.Applicative
>import safe Control.Monad
>import safe GHC.Generics hiding ((:*:), (:+:), R)
>import safe Data.Data
>import safe Data.Typeable
>import safe Data.Complex
>import safe Data.Sequence (Seq)
>import safe Data.Binary
>import safe qualified Data.Sequence as Seq
>import safe qualified Data.Monoid as Mon
>import safe Math.Tools.Universe
>import safe Math.Tools.Functor
>import safe Math.Number.Interface
>import safe Math.Matrix.Interface
>import safe Math.Matrix.Covector
>import safe Math.Tools.CoMonad
>import safe Math.Tools.Median
>import safe Math.Tools.NaturalTransformation
>import safe Math.Tools.Orthogonal hiding (outer3)
>import safe Math.Tools.Visitor
>import safe Math.Tools.PrettyP
>import safe Math.Matrix.Matrix
>import safe Math.Tools.Prop
>import safe Math.Number.Group
>import safe Math.Matrix.Vector1
>import safe Math.Matrix.Vector2
>import safe Math.Matrix.Indexable
>import safe Math.Matrix.Simple
>import safe Math.Matrix.FiniteVector
>import safe qualified Math.Number.Stream as Stream
>import safe Math.Number.Stream (Limiting(..), Infinitesimal(..), Stream(..), Closed(..))
>import safe Math.Number.Real

>-- | Three element vector
>data Vector3 s = Vector3 { 
>   xcoord3 :: s,
>   ycoord3 :: s,
>   zcoord3 :: s }
>       deriving (Eq, Typeable, Data, Generic)

>-- | this computes partial derivates of the scalar-valued 3D vector field
>-- along each variable simultaneously.
>-- \[\nabla f({\mathbb{v}}) = \frac{\partial \mathbb{f}}{\partial x}{\mathbb{i}}
>--                     + \frac{\partial \mathbb{f}}{\partial y}{\mathbb{j}}
>--                     + \frac{\partial \mathbb{f}}{\partial z}{\mathbb{k}}\]
>del_vector3 :: (Infinitesimal a)
>  => (Vector3 a -> a) -> Vector3 a -> Vector3 (Closure a)
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

>partial_derivate_vector3 :: (Infinitesimal (Scalar a)
> , VectorSpace a, Num a, Limiting a)
>   => (Vector3 (Scalar a) -> a) -> Vector3 (Scalar a) -> Closure (Vector3 a)
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

>cov3 :: (a ~ Scalar a) => Vector3 (Dual (Vector3 a))
>cov3 = Vector3 (covector xcoord3) (covector ycoord3) (covector zcoord3)

>instance (a ~ Scalar a) => ProjectionDual Vector3 a where
>   projection_dual = cov3

>instance (Binary s) => Binary (Vector3 s) where
>   put (Vector3 x y z) = put x >> put y >> put z
>   get = do { x <- get ; y <- get ; z <- get ; return $! (Vector3 x y z) }

>type ComplexVector3 a = (Vector3 :*: Complex) a

>instance Unfoldable Vector3 where
>   unfoldF f = f >>= \a -> f >>= \b -> f >>= \c -> return $! Vector3 a b c

>i3 :: (Num a) => Vector3 a
>i3 = identity <!> (xcoord3,id)

>j3 :: (Num a) => Vector3 a
>j3 = identity <!> (ycoord3,id)
> 
>k3 :: (Num a) => Vector3 a
>k3 = identity <!> (zcoord3,id)

>instance CircularComonad Vector3 where
>   rotate (Vector3 x y z) = Vector3 y z x
>   
>instance FiniteComonad Vector3 where
>   inverseRotate (Vector3 x y z) = Vector3 z x y

>instance (Universe a) => Universe (Vector3 a) where
>   all_elements = Vector3 <$> all_elements <*> all_elements <*> all_elements

>instance Comonad Vector3 where
>   extract (Vector3 x _ _) = x
>   duplicate (Vector3 x y z) = Vector3 (Vector3 x y z)
>                                       (Vector3 y z x)
>                                       (Vector3 z x y)

>instance Coapplicative Vector3 where
>   coeval (Vector3 x _ _) = x
>   colambda f = Vector3 (\a -> xcoord3 $ f $ Vector3 a a a)
>                        (\b -> ycoord3 $ f $ Vector3 b b b)
>                        (\c -> zcoord3 $ f $ Vector3 c c c)

>-- | <https://en.wikipedia.org/wiki/Dual_space#Injection_into_the_double-dual>

>instance (Num a, a ~ Scalar a) => FiniteDimensional (Vector3 a) where
>   finite (Matrix (Covector f)) = Vector3
>                                    (f (covector xcoord3))
>                                    (f (covector ycoord3))
>                                    (f (covector zcoord3)) 

>type Matrix3 a = (Vector3 :*: Vector3) a


>vec3 :: (a,a,a) -> Vector3 a
>vec3 (x,y,z) = Vector3 x y z 

>finite3 :: Vector3 a -> Vec 3 a
>finite3 (Vector3 x y z) = Cons x (Cons y (Cons z Empty))

>vector3_to_vec3 :: Vector3 a -> ThreeD -> a
>vector3_to_vec3 (Vector3 x y z) = svec3 x y z

>vec3_iso_obj :: Vector3 ThreeD
>vec3_iso_obj = Vector3 ThreeD0 ThreeD1 ThreeD2

>vec3_to_vector3 :: (ThreeD -> a) -> Vector3 a
>vec3_to_vector3 f = fmap f vec3_iso_obj

>vector3_natiso :: (->) ThreeD :<~>: Vector3
>vector3_natiso = NaturalIso (NatTrans vec3_to_vector3)
>                            (NatTrans vector3_to_vec3)

>constant3 :: a -> Vector3 a
>constant3 x = Vector3 x x x

>vec3Unit :: a -> Scalar (Vector3 a)
>vec3Unit x = x

>vec3Counit :: Vector3 (Scalar (Vector3 a)) -> Vector3 a
>vec3Counit (Vector3 x y z) = Vector3 x y z

>destructvec3 :: Vector3 a -> (a,a,a)
>destructvec3 (Vector3 x y z) = (x,y,z)

>setx3 :: s -> Vector3 s -> Vector3 s
>setx3 x v = v { xcoord3 = x }

>sety3 :: s -> Vector3 s -> Vector3 s
>sety3 y v = v { ycoord3 = y }

>setz3 :: s -> Vector3 s -> Vector3 s
>setz3 z v = v { zcoord3 = z }

>set_endo :: Vector3 s -> Vector3 (Mon.Endo (Vector3 s))
>set_endo = fmap Mon.Endo . set_vector_action

>set_vector_action :: Vector3 s -> Vector3 (Vector3 s -> Vector3 s)
>set_vector_action (Vector3 x y z) = Vector3 (setx3 x) (sety3 y) (setz3 z)

>removex3 :: Vector3 a -> Vector2 a
>removex3 (Vector3 _ y z) = Vector2 y z

>removey3 :: Vector3 a -> Vector2 a
>removey3 (Vector3 x _ z) = Vector2 x z

>removez3 :: Vector3 a -> Vector2 a
>removez3 (Vector3 x y _) = Vector2 x y

>remove3 :: Vector3 (Vector3 a -> Vector2 a)
>remove3 = Vector3 removex3 removey3 removez3

>remove_index3 :: Matrix3 a -> Matrix3 (Matrix2 a)
>remove_index3 (Matrix m) = matrix app remove3 remove3
>   where app f g = Matrix $ f $ fmap g m

>cofactor3 :: (Num a) => Matrix3 a -> Matrix3 a
>cofactor3 m = pure (*) <*> fmap fromIntegral signs3 <*> fmap determinant (remove_index3 m)

>-- | <https://en.wikipedia.org/wiki/Adjugate_matrix>

>adjucate3 :: (Num a) => Matrix3 a -> Matrix3 a
>adjucate3 = transpose . cofactor3

>-- | <https://en.wikipedia.org/wiki/Invertible_matrix>

>inverse3 :: (Fractional a) => Matrix3 a -> Matrix3 a
>inverse3 m = (1 / determinant m) %* adjucate3 m

>set3 :: Vector3 a -> Vector3 a -> (Vector3 :*: Vector3) a
>set3 (Vector3 x y z) v = Matrix $ Vector3 (setx3 x v) (sety3 y v) (setz3 z v)

>-- | <http://en.wikipedia.org/wiki/Elementary_matrix#Operations>

>swap3 :: (Vector3 :*: Vector3) (Vector3 a -> Vector3 a)
>swap3 = Matrix $ Vector3 (Vector3 id swapxy3 swapxz3)
>                         (Vector3 swapxy3 id swapyz3)
>                         (Vector3 swapxz3 swapyz3 id)

>mul3 :: (VectorSpace s) => Scalar s -> Vector3 (Vector3 s -> Vector3 s)
>mul3 p = Vector3 (\ (Vector3 x y z) -> Vector3 (p %* x) y z)
>                 (\ (Vector3 x y z) -> Vector3 x (p %* y) z)
>                 (\ (Vector3 x y z) -> Vector3 x y (p %* z))

>add3Matrix :: (VectorSpace a) => Scalar a 
>         -> (Vector3 :*: Vector3 :*: Vector3) a
>         -> (Vector3 :*: Vector3 :*: Vector3) a
>add3Matrix p m = Matrix (add3 p <*> (cells m))

>add3 :: (VectorSpace s)
>      => Scalar s -> (Vector3 :*: Vector3) (Vector3 s -> Vector3 s)
>add3 k = fmap ($ k) $ Matrix $ Vector3 (Vector3 addxx3 addxy3 addxz3)
>                                       (Vector3 addyx3 addyy3 addyz3)
>                                       (Vector3 addzx3 addzy3 addzz3)
>   where addxx3 k (Vector3 x y z) = Vector3 (x %+ k %* x) y z
>         addyy3 k (Vector3 x y z) = Vector3 x (y %+ k %* y) z
>         addzz3 k (Vector3 x y z) = Vector3 x y (z %+ k %* z)
>         addxy3 k (Vector3 x y z) = Vector3 (x %+ k %* y) y z
>         addyx3 k (Vector3 x y z) = Vector3 x (y %+ k%*x) z
>         addxz3 k (Vector3 x y z) = Vector3 (x %+ k %* z) y z
>         addzx3 k (Vector3 x y z) = Vector3 x y (z %+ k %* x)
>         addyz3 k (Vector3 x y z) = Vector3 x (y %+ k %* z) z
>         addzy3 k (Vector3 x y z) = Vector3 x y (z %+ k %* y)

>swapxy3 :: Vector3 a -> Vector3 a
>swapxy3 (Vector3 x y z) = Vector3 y x z

>swapxz3 :: Vector3 a -> Vector3 a
>swapxz3 (Vector3 x y z) = Vector3 z y x

>swapyz3 :: Vector3 a -> Vector3 a
>swapyz3 (Vector3 x y z) = Vector3 x z y

>mul3Matrix :: (Num a,VectorSpace a) => Scalar a -> Vector3 ((Vector3 :*: Vector3) a)
>mul3Matrix = fmap functionMatrix . mul3

>signs3 :: (Integral a) => Matrix3 a
>signs3 = fmap (\ (i,j) -> ((i+j+1) `mod` 2) * 2 - 1) matrix_indices3

>instance (Ord a) => Ord (Vector3 a) where
>  (Vector3 x y z) <= (Vector3 x' y' z') = x <= x' && y <= y' && z <= z'

>instance Visitor (Vector3 a) where
>  data Fold (Vector3 a) b = Vector3Fold (a -> a -> a -> b)
>  visit (Vector3Fold f) (Vector3 x y z) = f x y z

>instance MetricSpace (Vector3 R) where
>  distance (Vector3 x y z) (Vector3 x' y' z') = sqrt $
>       (x'-x)*(x'-x) + (y'-y)*(y'-y) + (z'-z)*(z'-z)

>instance AppendableVector Vector2 Vector1 where
>  type (Vector2 :+: Vector1) = Vector3
>  (Vector2 x y) |> (Vector1 z) = Vector3 x y z

>instance SplittableVector Vector2 Vector1 where
>  vsplit (Vector3 x y z) = (Vector2 x y,Vector1 z)

>instance AppendableVector Vector1 Vector2 where
>  type (Vector1 :+: Vector2) = Vector3
>  (Vector1 x) |> (Vector2 y z) = Vector3 x y z

>instance AppendableVector Vector3 Stream where
>  type (Vector3 :+: Stream) = Stream
>  (Vector3 x y z) |> a = x `Pre` y `Pre` z `Pre` a

>instance SplittableVector Vector1 Vector2 where
>  vsplit (Vector3 x y z) = (Vector1 x, Vector2 y z)

>instance Foldable Vector3 where
>   foldMap f (Vector3 x y z) = f x `mappend` f y `mappend` f z

>instance Traversable Vector3 where
>   traverse f (Vector3 x y z) = Vector3 <$> f x <*> f y <*> f z

>vector_epsilon :: (Infinitesimal a) => Stream (Vector3 a)
>vector_epsilon = epsilon_stream Stream.>>!= \x ->
>   epsilon_stream Stream.>>!= \y ->
>   epsilon_stream Stream.>>!= \z ->
>   return $! Vector3 x y z

>instance (ShowPrecision s) => ShowPrecision (Vector3 s) where
>   show_at_precision (Vector3 x y z) p =
>     "(" ++ show_at_precision x p ++ "," ++
>            show_at_precision y p ++ "," ++
>            show_at_precision z p ++ ")"

>instance (Show (f a)) => Show ((Vector3 :*: f) a) where
>  show (Matrix (Vector3 a b c))
>   = show a ++ "\n" ++ show b ++ "\n" ++ show c

>instance (MedianAlgebra s) => MedianAlgebra (Vector3 s) where
>  med (Vector3 a b c) (Vector3 a' b' c') (Vector3 a'' b'' c'')
>   = Vector3 (med a a' a'') (med b b' b'') (med c c' c'')

>instance (Show a) => Show (Vector3 a) where
>  show (Vector3 x y z) = "[" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "]"

>instance (Read a) => Read (Vector3 a) where
>  readsPrec i str = do pre <- char '[' str
>                       (x,xr) <- readsPrec 0 pre
>                       xr_ <- char ',' xr
>                       (y,yr) <- readsPrec 0 xr_
>                       yr_ <- char ',' yr
>                       (z,zr) <- readsPrec 0 yr_
>                       zr_ <- char ']' zr 
>                       return $! (Vector3 x y z,zr_)
>   where char ch (ch2:cr) | ch == ch2 = return cr
>                          | otherwise = []
>         char ch [] = []

>instance (PpShow a) => PpShow (Vector3 a) where
>  pp (Vector3 x y z) = vcat $ liftA2 nest [0,25,50] (map pp [x,y,z])

>instance PpShowVerticalF Vector3 where
>   ppf_vertical (Vector3 x y z) = pp '[' <> vcat [pp x,pp y, pp z] <> pp ']'
>instance PpShowF Vector3 where
>   ppf (Vector3 x y z) = pp '[' Mon.<> (sep [pp x,pp ',', pp y, pp ',',pp z]) Mon.<> pp ']'

instance (PpShow (f a)) => PpShow ((Vector3 :*: f) a) where
  pp (Matrix (Vector3 x y z)) = verticalize $ map pp [x,y,z]

>instance Functor Vector3 where
>  fmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

>instance Monad Vector3 where
>  return x = Vector3 x x x
>  x >>= f = diagonal3 $ Matrix $ fmap f x

>instance (Num s) => Semigroup (Vector3 s) where
>   (<>) = (%+)

>instance (Num s) => Mon.Monoid (Vector3 s) where
>  mempty = vzero
>  mappend = (%+)
>  
>-- | see "Lawvere,Rosebrugh: Sets for mathematics", pg. 167.
>instance (ConjugateSymmetric a, Num a) => Semigroup ((Vector3 :*: Vector3) a) where
>   (<>) = (%**%)


>-- | see "Lawvere,Rosebrugh: Sets for mathematics", pg. 167.
>instance (Num a, ConjugateSymmetric a) => Mon.Monoid ((Vector3 :*: Vector3) a) where
>   mempty  = identity
>   mappend = (%**%)

>instance (Num s) => Group (Vector3 s) where
>   ginvert (Vector3 x y z) = Vector3 (negate x) (negate y) (negate z)

>instance (Fractional a, ConjugateSymmetric a) => Group ((Vector3 :*: Vector3) a) where
>   ginvert = inverse

>instance (Limiting a) => Limiting (Vector3 a) where
>  data Closure (Vector3 a) = Vector3Closure (Vector3 (Closure a))
>  limit str = Vector3Closure $ Vector3
>                      (limit $ fmap xcoord3 str)
>                      (limit $ fmap ycoord3 str)
>                      (limit $ fmap zcoord3 str)
>  approximations (Vector3Closure (Vector3 a b c)) = do
>     (a',b',c') <- fzip3 (approximations a)
>                         (approximations b)
>                         (approximations c)
>     return $! Vector3 a' b' c'

>instance (Show a, Limiting a) => Show (Closure (Vector3 a)) where
>   showsPrec _ v = shows $ shead $ approximations v

>instance (PpShow a, Limiting a) => PpShow (Closure (Vector3 a)) where
>   pp v = pp $ shead $ approximations v


approximations_vector3 :: Vector3 R -> Stream (Vector3 R)
approximations_vector3 (Vector3 x y z) = do
        (x',(y',z')) <- approximate x `zip` (approximate y `zip` approximate z)
        return $ Vector3 (Limit x') (Limit y') (Limit z')

>-- | partial derivate a function defined for each coordinate along
>-- each dimension of three-dimensional vector.
>pderive3 :: (Closed a, Infinitesimal a)
>   => Vector3 (a -> a) -> Vector3 a -> Vector3 a
>pderive3 (Vector3 fx fy fz) (Vector3 x y z) = Vector3
>   (partial_derive chx fx x)
>   (partial_derive chy fy y)
>   (partial_derive chz fz z)
>  where chx eps x = x + eps
>        chy eps y = y + eps
>        chz eps z = z + eps

>vector_field_derivate :: (Closed a, Infinitesimal a)
>  => (Vector3 a -> a) -> Vector3 a -> Vector3 a
>vector_field_derivate f v@(Vector3 x y z) = Vector3
>   (partial_derivate dx3 f v)
>   (partial_derivate dy3 f v)
>   (partial_derivate dz3 f v)

>dx3_endo :: (Num a) => a -> Mon.Endo (Vector3 a)
>dx3_endo = Mon.Endo . dx3
>dy3_endo :: (Num a) => a -> Mon.Endo (Vector3 a)
>dy3_endo = Mon.Endo . dy3
>dz3_endo :: (Num a) => a -> Mon.Endo (Vector3 a)
>dz3_endo = Mon.Endo . dz3

>dx3 :: (Num a) => a -> Vector3 a -> Vector3 a
>dx3 eps = \case { (Vector3 x y z) -> Vector3 (x + eps) y z }
> 
>dy3 :: (Num a) => a -> Vector3 a -> Vector3 a
>dy3 eps = \case { (Vector3 x y z) -> Vector3 x (y + eps) z }
> 
>dz3 :: (Num a) => a -> Vector3 a -> Vector3 a
>dz3 eps = \case { (Vector3 x y z) -> Vector3 x y (z + eps) }

>partial_derivate3x :: (Infinitesimal a, Closed a)
>                   => Dual (Vector3 a) -> Dual (Vector3 a)
>partial_derivate3x (Covector f) = covector $ partial_derivate dx3 f

>partial_derivate3y :: (Infinitesimal a, Closed a) => Dual (Vector3 a) -> Dual (Vector3 a)
>partial_derivate3y (Covector f) = covector $ partial_derivate dy3 f
          
>partial_derivate3z :: (Infinitesimal a, Closed a) => Dual (Vector3 a) -> Dual (Vector3 a)
>partial_derivate3z (Covector f) = covector $ partial_derivate dz3 f

>-- | \[\nabla_3\], three-dimensional partial derivate. Use Applicative.<*> for applying it.
>del3 :: (Infinitesimal v, Closed v) => Vector3 (Dual (Vector3 v) -> Dual (Vector3 v))
>del3 = Vector3 partial_derivate3x partial_derivate3y partial_derivate3z


>-- outer product \[\nabla_3 \otimes \nabla_3\] operator for three-dimensional vectors.
>del3_squared :: (v ~ Scalar v, Infinitesimal v, Closed v)
>  => (Vector3 :*: Vector3) (Dual (Vector3 v) -> Dual (Vector3 v))
>del3_squared = matrix (.) del3 del3

>op_dot :: (Num b) => Vector3 (a -> b) -> a -> b
>op_dot (Vector3 f g h) z = f z + g z + h z

>hessian3 :: (v ~ Scalar v,Infinitesimal v, Closed v)
> => Dual (Vector3 v) -> (Vector3 :*: Vector3) (Dual (Vector3 v))
>hessian3 f = matrix (\a b -> a (b f)) del3 del3

>instance (Infinitesimal a, Closed a) => VectorDerivative (Vector3 a) where
>  divergence = divergence3
>  grad = grad3

>instance (Infinitesimal a, Closed a) => VectorCrossProduct (Vector3 a) where
>  curl = curl3

>instance (Infinitesimal a, Closed a) => VectorLaplacian (Vector3 a) where
>  vector_laplace = vector_laplace3

>vector_laplace3 :: (VectorDerivative v) =>
>  LinearMap v (Vector3 (Scalar v)) -> LinearMap v (Vector3 (Scalar v))
>vector_laplace3 f = LinearMap $ \x -> Vector3
> ((laplace $ linear_dual_3x f) `bracket` x)
> ((laplace $ linear_dual_3y f) `bracket` x)
> ((laplace $ linear_dual_3z f) `bracket` x)

>-- | <https://en.wikipedia.org/wiki/Divergence>
>divergence3 :: (Infinitesimal a, Closed a)
> => LinearMap (Vector3 a) (Vector3 a) -> Dual (Vector3 a)
>divergence3 f = partial_derivate3x (linear_dual_3x f)
>              + partial_derivate3y (linear_dual_3y f)
>              + partial_derivate3z (linear_dual_3z f)

>linear_dual_3x :: LinearMap v (Vector3 (Scalar v)) -> Dual v
>linear_dual_3x f = covector (xcoord3 . (-!<) f)

>linear_dual_3y :: LinearMap v (Vector3 (Scalar v)) -> Dual v
>linear_dual_3y f = covector (ycoord3 . (-!<) f)

>linear_dual_3z :: LinearMap v (Vector3 (Scalar v)) -> Dual v
>linear_dual_3z f = covector (zcoord3 . (-!<) f)

>cycle3 :: Vector3 a -> Stream a
>cycle3 z@(Vector3 a b c) = a `Pre` b `Pre` c `Pre` cycle3 z


>-- | <https://en.wikipedia.org/wiki/Gradient>
>grad3 :: (Infinitesimal s, Closed s) => Dual (Vector3 s) -> LinearMap (Vector3 s) (Vector3 s)
>grad3 f = LinearMap $ \z -> Vector3 (partial_derivate3x f `bracket` z)
>                                    (partial_derivate3y f `bracket` z)
>                                    (partial_derivate3z f `bracket` z)

>-- | <https://en.wikipedia.org/wiki/Laplace_operator>
>laplace3 :: (Infinitesimal a, Closed a) => Dual (Vector3 a) -> Dual (Vector3 a)
>laplace3 f = divergence3 (grad3 f)


>-- | <https://en.wikipedia.org/wiki/Curl_(mathematics)>

>curl3 :: (Infinitesimal a, Closed a) => LinearMap (Vector3 a) (Vector3 a)
>                           -> LinearMap (Vector3 a) (Vector3 a)
>curl3 f = LinearMap $ \z -> Vector3 ((partial_derivate3y fz - partial_derivate3z fy)
>                       `bracket` z)
>                    ((partial_derivate3z fx - partial_derivate3x fz)
>                       `bracket` z)
>                    ((partial_derivate3x fy - partial_derivate3y fx)
>                       `bracket` z)
>  where fx = linear_dual_3x f
>        fy = linear_dual_3y f
>        fz = linear_dual_3z f

>instance Applicative Vector3 where
>   pure x = Vector3 x x x
>   (Vector3 f g h) <*> (Vector3 x y z) = Vector3 (f x) (g y) (h z)

>instance (Num a) => VectorSpace (Vector3 a) where
>  type Scalar (Vector3 a) = a
>  vzero = pure 0
>  vnegate = liftA negate
>  v %* w = liftA (v*) w
>  (%+) = liftA2 (+)

>instance (ConjugateSymmetric a) => ConjugateSymmetric (Vector3 a) where
>   conj (Vector3 x y z) = Vector3 (conj x) (conj y) (conj z)

>-- | <https://en.wikipedia.org/wiki/Conjugate_transpose>
>instance (ConjugateSymmetric a) => ConjugateSymmetric ((Vector3 :*: Vector3) a) where
>   conj = fmap conj . transpose

>instance (Floating a, ConjugateSymmetric a) => NormedSpace (Vector3 a) where
>  norm = innerproductspace_norm 

>instance (ConjugateSymmetric a, Num a) => InnerProductSpace (Vector3 a) where
>  (Vector3 x y z) %. (Vector3 x' y' z') = x * conj x' + y * conj y' + z * conj z'

>sum_coordinates3 :: (Num a) => Vector3 a -> a
>sum_coordinates3 (Vector3 x y z) = x + y + z

>-- | cube root of a sum of cubes.
>cube_norm3 :: (Floating a) => Vector3 a -> a
>cube_norm3 (Vector3 x y z) = let n = 3 in (x**n + y**n + z**n)**(1/n)

>-- | nth root of a sum of nth powers
>nth_norm3 :: (Floating a) => a -> Vector3 a -> a
>nth_norm3 n (Vector3 x y z) = (x**n + y**n + z**n)**(1/n)

>instance Summable Vector3 where
>  sum_coordinates = sum_coordinates3

>instance (Num a) => SquareMatrix Vector3 a where
>  identity = identity3
>  diagonal = diagonal3
>  diagonal_matrix = diagonal_matrix3

>instance (Num a) => FiniteSquareMatrix Vector3 a where
>  trace = trace3 
>  determinant = determinant3 

>instance Transposable Vector3 Vector2 where
>  transpose (Matrix (Vector3 (Vector2 x1 y1) (Vector2 x2 y2) (Vector2 x3 y3)))
>    = Matrix $ Vector2 (Vector3 x1 x2 x3) (Vector3 y1 y2 y3)

>instance Transposable Vector2 Vector3 where
>  transpose (Matrix (Vector2 (Vector3 x1 x2 x3) (Vector3 y1 y2 y3)))
>    = Matrix $ Vector3 (Vector2 x1 y1) (Vector2 x2 y2) (Vector2 x3 y3)

>instance Transposable Vector3 Vector1 where
>  transpose (Matrix x) = Matrix $ Vector1 (fmap vector_element x)

>instance Transposable Vector1 Vector3 where
>  transpose (Matrix (Vector1 x)) = Matrix $ fmap Vector1 x

>instance Transposable Vector3 Vector3 where
>  transpose = transpose3

>instance (Num a) => LinearTransform Vector3 Vector3 a where
>  (<*>>) = left_multiply3
>  (<<*>) = right_multiply3

>instance (Num a) => CoordinateSpace (Vector3 a) where
>  type Coordinate (Vector3 a) = Int
>  index 0 = xcoord3
>  index 1 = ycoord3
>  index 2 = zcoord3
>  listVector = vector3
>  dimension_size _ = 3
>  coordinates _ = [0,1,2]

>instance (Num a) => StandardBasis (Vector3 a) where
>  unit_vectors = [xid,yid,zid]

>instance (Num a, ConjugateSymmetric a) => Num ((Vector3 :*: Vector3) a) where
>  (Matrix v) + (Matrix v') = Matrix $ liftA2 (liftA2 (+)) v v'
>  (Matrix v) - (Matrix v') = Matrix $ liftA2 (liftA2 subtract) v v'
>  (*) = (%*%)
>  negate (Matrix v) = Matrix (negate v)
>  abs (Matrix v) = Matrix (abs v)
>  signum (Matrix v) = Matrix (signum v)
>  fromInteger = diagonal_matrix . constant3 . fromInteger

>instance (Fractional a, ConjugateSymmetric a) => Fractional ((Vector3 :*: Vector3) a) where
>   recip = inverse
>   fromRational = diagonal_matrix . constant3 . fromRational

>-- | <http://en.wikipedia.org/wiki/Hyperbolic_function hyperbolic function>
>instance (Floating a, Closed a, ConjugateSymmetric a) => Floating ((Vector3 :*: Vector3) a) where
>   pi = error "Matrix pi is not implemented."
>   exp = matrix_exponential3
>   log = error "Matrix log is not implemented."
>   cos = error "Matrix cosine is not implemented."
>   sin = error "Matrix sine is not implemented."
>   tan = error "Matrix tangent is not implemented."
>   sinh x = (exp x - exp (negate x)) / 2
>   cosh x = (exp x + exp (negate x)) / 2
>   tanh x = sinh x / cosh x
>   acos = error "Matrix acos is not implemented."
>   asin = error "Matrix asin is not implemented."
>   atan = error "Matrix atan is not implemented."
>   asinh = error "Matrix asinh is not implemented."
>   acosh = error "Matrix acosh is not implemented."
>   atanh = error "Matrix atanh is not implemented."

>instance (Num a) => Num (Vector3 a) where
>  (Vector3 x y z) + (Vector3 x' y' z') = Vector3 (x+x') (y+y') (z+z')
>  (Vector3 x y z) - (Vector3 x' y' z') = Vector3 (x-x') (y-y') (z-z')
>  (*) = cross_product
>  negate (Vector3 x y z) = Vector3 (negate x) (negate y) (negate z)
>  abs (Vector3 x y z) = Vector3 (abs x) (abs y) (abs z)
>  signum (Vector3 x y z) = Vector3 (signum x) (signum y) (signum z)
>  fromInteger i = error "fromInteger: Vector3 requires 3 components"

>index3 :: Vector3 (Vector3 a -> a)
>index3 = Vector3 xcoord3 ycoord3 zcoord3

>mapBasis :: (Num a) => (Vector3 a -> Vector3 a) -> Vector3 a -> Vector3 a
>mapBasis f (Vector3 x y z) = x %* f i_vec + y %* f j_vec + z %* f k_vec

>functionMatrix3 :: (Num a) => (Vector3 a -> Vector3 a) -> Matrix3 a
>functionMatrix3 f = Matrix $ Vector3 (f xid) (f yid) (f zid)

>splitx,splity,splitz :: Vector3 s -> (Vector2 s, Vector1 s)
>splitz (Vector3 x y z) = (Vector2 x y, Vector1 z)
>splitx (Vector3 x y z) = (Vector2 y z, Vector1 x)
>splity (Vector3 x y z) = (Vector2 z x, Vector1 y)

>trace3 :: (Num a) => Matrix3 a -> a
>trace3 = sum_coordinates3 . diagonal3

>vector3 :: [a] -> Vector3 a
>vector3 [x,y,z] = Vector3 x y z
>vector3 _ = error "vector3: Invalid number of items in vector"

>-- | <https://en.wikipedia.org/wiki/Lie_algebra>
>instance (Num a) => LieAlgebra (Vector3 a) where
>   (%<>%) = cross_product

>instance (Num a, ConjugateSymmetric a) => LieAlgebra ((Vector3 :*: Vector3) a) where
>   (%<>%) = matrix_commutator

>-- | <https://en.wikipedia.org/wiki/Cross_product>
>-- WARNING: a cross product produces an axial vector.
>-- <https://en.wikipedia.org/wiki/Pseudovector>
>cross_product :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
>cross_product (Vector3 u1 u2 u3) (Vector3 v1 v2 v3) =
>      Vector3 (u2*v3 - u3*v2) (u3*v1 - u1*v3) (u1*v2 - u2*v1)

>-- | cross_product3 computes the three dimensional vector that
>-- is orthogonal to both rows of the 2x3 vector.
>cross_product3 :: (Num a) => (Vector2 :*: Vector3) a -> Vector3 a
>cross_product3 (Matrix (Vector2 a b)) = cross_product a b

>matrix_exponential3 :: (Closed b, ConjugateSymmetric b, Fractional b)
>   => Matrix3 b -> Matrix3 b
>matrix_exponential3 m = Stream.ssum $ 
>    liftA2 (\x y -> fmap (/y) x)
>               (let v = Pre identity3 (fmap (m `matrix_multiply3`) v) in v)
>               Stream.factorial

>xid,yid,zid :: (Num a) => Vector3 a
>xid = Vector3 1 0 0
>yid = Vector3 0 1 0
>zid = Vector3 0 0 1

>identity3 :: (Num a) => Matrix3 a
>identity3 = Matrix $ Vector3 xid yid zid


>matrix_power3 :: (ConjugateSymmetric a,Num a) => Matrix3 a -> Integer -> Matrix3 a
>matrix_power3 mat 0 = identity3
>matrix_power3 mat i = mat `matrix_multiply3` matrix_power3 mat (pred i)

>vector_indices3 :: (Integral a) => Vector3 a
>vector_indices3 = Vector3 1 2 3

>matrix_indices3 :: (Integral a) => (Vector3 :*: Vector3) (a,a)
>matrix_indices3 = matrix (,) vector_indices3 vector_indices3

>matrix_multiply3 :: (ConjugateSymmetric a,Num a) => Matrix3 a -> Matrix3 a -> Matrix3 a
>matrix_multiply3 (Matrix m1) m2 
>   | Matrix m2t <- transpose3 m2 = matrix (%.) m1 m2t

>-- | dot3 doesn't work on complex numbers

>dot3 :: (Num a) => Vector3 a -> Vector3 a -> a
>dot3 x y = sum_coordinates3 $ liftA2 (*) x y

>left_multiply3 :: (Num a) => Vector3 a -> Matrix3 a -> Vector3 a
>left_multiply3 v (Matrix m) = fmap (dot3 v) m

>right_multiply3 :: (Num a) => Matrix3 a -> Vector3 a -> Vector3 a
>right_multiply3 (Matrix m) v = fmap (dot3 v) m
             
>transpose3 :: Matrix3 a -> Matrix3 a
>transpose3 (Matrix m) = matrix ($) diagonal_projections3 m

>outer3 :: (a -> b -> c) -> Vector3 a -> Vector3 b -> Matrix3 c
>outer3 f (Vector3 x y z) v
>  = Matrix $ Vector3 (fmap (f x) v) (fmap (f y) v) (fmap (f z) v)

>zipWithV3 :: (a -> b -> c) -> Vector3 a -> Vector3 b -> Vector3 c
>zipWithV3 = liftA2

>rotate_x :: (Floating a) => a -> Matrix3 a
>rotate_x alfa = Matrix $ Vector3 (Vector3 1 0 0)
>                        (Vector3 0 (cos alfa) (-sin alfa))
>                        (Vector3 0 (sin alfa) (cos alfa))

>rotate_y :: (Floating a) => a -> Matrix3 a
>rotate_y alfa = Matrix $ Vector3 (Vector3 (cos alfa) 0 (sin alfa))
>                        (Vector3 0 1 0)
>                        (Vector3 (-sin alfa) 0 (cos alfa))

>rotate_z :: (Floating a) => a -> Matrix3 a
>rotate_z alfa = Matrix $ Vector3 (Vector3 (cos alfa) (-sin alfa) 0)
>                        (Vector3 (sin alfa) (cos alfa) 0)
>                        (Vector3 0 0 1)

>rotate_coordinates3 :: Vector3 a -> Vector3 a
>rotate_coordinates3 (Vector3 x y z) = Vector3 y z x

>zero_codiagonal3 :: (Num a) => Codiagonal Vector3 a
>zero_codiagonal3 = Codiagonal3 vzero vzero zero_codiagonal2

>diagonal_matrix3 :: (Num a) => Vector3 a -> (Vector3 :*: Vector3) a
>diagonal_matrix3 v = matrix3 v zero_codiagonal3

>matrix3 :: Vector3 a -> Codiagonal Vector3 a -> (Vector3 :*: Vector3) a
>matrix3 (Vector3 d1 d2 d3) (Codiagonal3 (Vector2 a' a'') (Vector2 a b)
>                             (Codiagonal2 (Vector1 x) (Vector1 y))) = Matrix $
>   Vector3 (Vector3 d1 a b)
>           (Vector3 a' d2 y)
>           (Vector3 a'' x d3)

>codiag3 :: Matrix3 a -> Codiagonal Vector3 a
>codiag3 (Matrix (Vector3 (Vector3 _ a b)
>                            (Vector3 a' _ b')
>                            (Vector3 a'' b'' _))) =
>    Codiagonal3 (Vector2 a' a'')
>                (Vector2 a b) (Codiagonal2 (Vector1 b'') (Vector1 b'))


>codiagonal3 :: Matrix3 a -> Vector2 (Vector2 a, Vector2 a)
>codiagonal3 (Matrix (Vector3 (Vector3 _ a b)
>                            (Vector3 a' _ b')
>                            (Vector3 a'' b'' _))) = 
>                   Vector2 (Vector2 a' a'' , Vector2 a b  )
>                           (Vector2 b'' b'', Vector2 b' b')

>instance ProjectionSpace Vector3 Vector1 where
>   data (Vector3 \\\ Vector1) a = S31Vector (Vector2 a)
>   project_first (Vector3 x _ _) = Vector1 x
>   project_second (Vector3 _ y z) = S31Vector $ Vector2 y z
>   join_vector (Vector1 x) (S31Vector (Vector2 y z)) = Vector3 x y z

>instance ProjectionSpace (Vector3 \\\ Vector1) Vector1 where
>   data ((Vector3 \\\ Vector1) \\\ Vector1) a = S311Vector (Vector1 a)
>   project_first (S31Vector (Vector2 x _)) = Vector1 x
>   project_second (S31Vector (Vector2 _ y)) = S311Vector (Vector1 y)
>   join_vector (Vector1 x) (S311Vector (Vector1 y)) = S31Vector $ Vector2 x y

>instance ProjectionSpace Vector3 Vector2 where
>   data (Vector3 \\\ Vector2) a = S32Vector (Vector1 a)
>   project_first (Vector3 x y _) = Vector2 x y
>   project_second (Vector3 _ _ z) = S32Vector $ Vector1 z
>   join_vector (Vector2 x y) (S32Vector (Vector1 z)) = Vector3 x y z

>instance (Show a) => Show ((Vector3 \\\ Vector2) a) where
>   show (S32Vector x) = show x

>instance Functor (Vector3 \\\ Vector2) where
>   fmap f (S32Vector v) = S32Vector (fmap f v)
>   
>instance Applicative (Vector3 \\\ Vector2) where
>   pure x = S32Vector (pure x)
>   (S32Vector f) <*> (S32Vector x) = S32Vector $ f <*> x

>instance (Show a) => Show ((Vector3 \\\ Vector1) a) where
>   show (S31Vector x) = show x

>instance Functor (Vector3 \\\ Vector1) where
>   fmap f (S31Vector v) = S31Vector (fmap f v)

>instance Applicative (Vector3 \\\ Vector1) where
>   pure x = S31Vector (pure x)
>   (S31Vector f) <*> (S31Vector x) = S31Vector $ f <*> x

>instance CodiagonalMatrix Vector3 a where
>   data Codiagonal Vector3 a = Codiagonal3 {
>      down_codiagonal3 :: Vector2 a,
>      right_codiagonal3 :: Vector2 a,
>      diagonal_codiagonal3 :: Codiagonal Vector2 a
>     }
>   type (Vector3 \\ a) = Vector2 a
>   codiagonal = codiag3
>   (|\|) = matrix3
>   down_project = down_codiagonal3
>   right_project = right_codiagonal3

>instance (Show a) => Show (Codiagonal Vector3 a) where
>   show (Codiagonal3 (Vector2 d1 d2) (Vector2 r1 r2)
>         (Codiagonal2 (Vector1 a1) (Vector1 a2))) =
>     "* " ++ show r1 ++ " " ++ show r2 ++ "\n" ++
>     show d1 ++ " * " ++ show a2 ++ "\n" ++
>     show d2 ++ " " ++ show a1 ++ " *"

deriving instance (Show a) => Show (Codiagonal Vector3 a)

>instance Functor (Codiagonal Vector3) where
>   fmap f (Codiagonal3 down right diag) =
>     Codiagonal3 (fmap f down) (fmap f right) (fmap f diag)

>instance Applicative (Codiagonal Vector3) where
>   pure x = Codiagonal3 (pure x) (pure x) (pure x)
>   (Codiagonal3 f1 f2 fr) <*> (Codiagonal3 x1 x2 xr) =
>     Codiagonal3 (f1 <*> x1) (f2 <*> x2) (fr <*> xr)

>instance Indexable Vector3 where
>  diagonal_projections = diagonal_projections3
>  indexable_indices = Vector3 0 1 2

>diagonal3 :: Matrix3 a -> Vector3 a
>diagonal3 (Matrix x) = diagonal_projections3 <*> x

>diagonal_projections3 :: Vector3 (Vector3 a -> a)
>diagonal_projections3 = Vector3 xcoord3 ycoord3 zcoord3

>diagonal3x :: Matrix3 a -> Vector3 a
>diagonal3x (Matrix x) = diagonal_projections3 <*> x
  
>diagonal3y :: Matrix3 a -> Vector3 a
>diagonal3y (Matrix x) = rotate_coordinates3 diagonal_projections3 <*> x
  
>diagonal3z :: Matrix3 a -> Vector3 a
>diagonal3z (Matrix x) = rotate_coordinates3 (rotate_coordinates3 diagonal_projections3) <*> x

>-- | algorithm from <http://en.wikipedia.org/wiki/Determinant>

>determinant3 :: (Num a) => Matrix3 a -> a
>determinant3 (Matrix z@(Vector3 (Vector3 a b c) _ _)) =
>      a * det removex3 - b * det removey3 + c * det removez3
>  where det rc = determinant (Matrix $ removex3 $ fmap rc z)

>determinant3_ :: (Num a) => Matrix3 a -> a
>determinant3_ (Matrix m) = combine $ pure (*) <*> xcoord3 m <*> amv
>   where amv = fmap (determinant . Matrix . removex3 . (`fmap` m)) remove3
>         combine (Vector3 a b c) = a - b + c         

>instance (Num a, ConjugateSymmetric a) => LinearTransform Vector1 Vector3 a where
>  x <*>> (Matrix (Vector1 m)) = Vector1 $ x %. m
>  (Matrix (Vector1 m)) <<*> (Vector1 x) = x %* m


>instance (Num a,ConjugateSymmetric a) => LinearTransform Vector3 Vector1 a where
>  (Vector1 x) <*>> (Matrix m) = x %* fmap vector_element m
>  (Matrix (Vector3 (Vector1 x) (Vector1 y) (Vector1 z))) <<*> (Vector3 a b c) = Vector1 (x*a+y*b+z*c) 

instance NumSpace (Vector3 (Complex R)) where
instance FractionalSpace (Vector3 (Complex R)) where

>-- | 1 x 3 matrices:

>instance (Num a) => VectorSpace ((Vector1 :*: Vector3) a) where
>  type Scalar ((Vector1 :*: Vector3) a) = a
>  vzero = Matrix $ Vector1 vzero 
>  vnegate (Matrix v) = Matrix $ fmap negate v
>  v %* (Matrix x) = Matrix $ fmap (fmap (v *)) x
>  (Matrix x) %+ (Matrix y) = Matrix $ x %+ y

>-- | 3 x 1 matrices:

>instance (Num a) => VectorSpace ((Vector3 :*: Vector1) a) where
>  type Scalar ((Vector3 :*: Vector1) a) = a
>  vzero = Matrix $ Vector3 vzero vzero vzero
>  vnegate (Matrix v) = Matrix $ fmap negate v
>  v %* (Matrix x) = Matrix $ fmap (fmap (v *)) x
>  (Matrix x) %+ (Matrix y) = Matrix $ x %+ y

>-- | 2 x 3 matrices:

>instance (Num a) => VectorSpace ((Vector2 :*: Vector3) a) where
>  type Scalar ((Vector2 :*: Vector3) a) = a
>  vzero = Matrix $ Vector2 vzero vzero
>  vnegate (Matrix v) = Matrix $ fmap negate v
>  v %* (Matrix x) = Matrix $ fmap (fmap (v *)) x
>  (Matrix x) %+ (Matrix y) = Matrix $ x %+ y

>-- | 3 x 2 matrices

>instance (Num a) => VectorSpace ((Vector3 :*: Vector2) a) where
>  type Scalar ((Vector3 :*: Vector2) a) = a
>  vzero = Matrix $ Vector3 vzero vzero vzero
>  vnegate (Matrix v) = Matrix $ fmap negate v
>  v %* (Matrix x) = Matrix $ fmap (fmap (v *)) x
>  (Matrix x) %+ (Matrix y) = Matrix $ x %+ y

>-- | 3 x 3 matrices:

>instance {-# OVERLAPPING #-}
>   (Num a) => VectorSpace ((Vector3 :*: Vector3) a) where
>  type Scalar ((Vector3 :*: Vector3) a) = a
>  vzero = Matrix $ Vector3 vzero vzero vzero
>  vnegate (Matrix v) = Matrix $ fmap negate v
>  v %* (Matrix x) = Matrix $ fmap (fmap (v *)) x
>  (Matrix x) %+ (Matrix y) = Matrix $ x %+ y

>instance {-# OVERLAPPING #-}
>     (Floating a, ConjugateSymmetric a) => NormedSpace ((Vector3 :*: Vector3) a) where
>  norm = innerproductspace_norm

>instance (Floating a, ConjugateSymmetric a) => InnerProductSpace ((Vector3 :*: Vector3) a) where
>  x %. y = trace (transpose x %*% y)

>gram_schmidt3 :: (Fractional (Scalar a), Num a,
>                  InnerProductSpace a, VectorSpace a)
>              => Vector3 a -> Vector3 a
>gram_schmidt3 (Vector3 x y z) = Vector3 u1 u2 u3
>   where u1 = x
>         u2 = y - projection u1 y
>         u3 = z - projection u1 z - projection u2 z

>-- | <https://en.wikipedia.org/wiki/Eigenvalue_algorithm>
>--   based on algorithm by
>--   Smith, Oliver K.: Eigenvalues of symmetric 3x3 matrix, 1961, Communications of the ACM., <doi:10.1145/355578.366316>
>--  The algorithm works reasonably when the matrix is real and symmetric.
>eigenvalue3 :: (Floating a, Ord a, ConjugateSymmetric a) => (Vector3 :*: Vector3) a -> Vector3 a
>eigenvalue3 m = if p1 == 0 then diagonal m else Vector3 eig1 eig2 eig3
>   where eig1 = q + 2*p*cos(phi)
>         eig3 = q + 2*p*cos(phi + 2*pi/3)
>         eig2 = 3*q - eig1 - eig3
>         pv = Vector3 (m <!> (xcoord3,ycoord3))
>                      (m <!> (xcoord3,zcoord3))
>                      (m <!> (ycoord3,zcoord3))
>         p1 = pv %. pv
>         q = trace m / 3
>         mdq = diagonal m - return q
>         p2 = mdq %. mdq + 2 * p1
>         p = sqrt (p2 / 6)
>         b = (1 / p) %* (m %- q %* identity)
>         r = determinant b / 2
>         phi | r <= -1   = pi / 3
>             | r >= 1    = 0
>             | otherwise = acos r / 3


>instance (Floating a, Ord a, ConjugateSymmetric a) => EigenDecomposable Vector3 a where
>   eigenvalues = eigenvalue3

>instance (Fractional a) => InvertibleMatrix Vector3 a where
>   cofactor = cofactor3
>   adjucate = adjucate3
>   inverse = inverse3
>
>-- | <https://en.wikipedia.org/wiki/Levi-Civita_symbol>
>levi_civita3 :: (Vector3 :*: Vector3 :*: Vector3) Int
>levi_civita3 = matrix m (matrix (,) indexable_indices indexable_indices) indexable_indices
>  where m (x,y) z | x == 0 && y == 1 && z == 2 = 1
>                  | x == 1 && y == 2 && z == 0 = 1
>                  | x == 2 && y == 0 && z == 1 = 1
>                  | x == 2 && y == 1 && z == 0 = negate 1
>                  | x == 0 && y == 2 && z == 1 = negate 1
>                  | x == 1 && y == 0 && z == 2 = negate 1
>                  | x == y || y == z || z == x = 0

