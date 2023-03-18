>{-# LANGUAGE Safe,FlexibleInstances, MultiParamTypeClasses,FlexibleContexts, TypeOperators, TypeFamilies, NoMonomorphismRestriction, StandaloneDeriving, DeriveGeneric, DeriveDataTypeable, LambdaCase #-}
>module Math.Matrix.Vector4 where
>import safe qualified Data.Monoid as Mon
>import safe qualified Text.PrettyPrint as Pretty
>import safe Text.PrettyPrint (nest,vcat)
>import safe Control.Applicative
>import safe Math.Tools.Functor
>import safe GHC.Generics hiding ((:+:), (:*:), R)
>import safe Data.Data
>import safe Data.Typeable
>import safe Math.Matrix.Points

import Math.Matrix.Dimension

>import safe Math.Tools.PrettyP
>import safe Math.Tools.Universe
>import safe Data.Complex
>import safe Math.Tools.Isomorphism
>import safe Math.Tools.Orthogonal
>import safe Math.Matrix.Interface
>import safe qualified Data.Binary as Bin
>import safe Math.Tools.Median
>import safe Math.Tools.Visitor
>import safe Math.Tools.CoMonad
>import safe Math.Matrix.Vector1
>import safe Math.Matrix.Vector2
>import safe Math.Matrix.Vector3
>import safe Math.Number.Interface
>import safe Math.Number.Group
>import safe Math.Number.StreamInterface
>import safe qualified Control.Monad.Zip

>deriving instance (Eq a) => Eq (Vector4 a)
>deriving instance (Typeable a) => Typeable (Vector4 a)
>deriving instance (Generic a) => Generic (Vector4 a)
>deriving instance (Data a) => Data (Vector4 a)

>instance (ShowPrecision s) => ShowPrecision (Vector4 s) where
>  show_at_precision (Vector4 t x y z) p =
>    "(" ++ show_at_precision t p ++ "," ++ show_at_precision x p ++ "," ++
>    show_at_precision y p ++ "," ++ show_at_precision z p ++ ")"

>instance (Bin.Binary s) => Bin.Binary (Vector4 s) where
>   put (Vector4 t x y z) = Bin.put t >> Bin.put x >> Bin.put y >> Bin.put z
>   get = do
>     t <- Bin.get
>     x <- Bin.get
>     y <- Bin.get
>     z <- Bin.get
>     return $! Vector4 t x y z



>instance PpShowVerticalF Vector4 where
>   ppf_vertical (Vector4 x y z t) = pp '[' <> vcat [pp x,pp y, pp z, pp t] <> pp ']'
>instance PpShowF Vector4 where
>   ppf (Vector4 x y z t) = pp '[' Mon.<> (Pretty.sep [pp x,pp ',', pp y, pp ',',pp z, pp ',', pp t]) Mon.<> pp ']'


>instance (Num a) => Num (Vector4 a) where
>   v1 + v2 = pure (+) <*> v1 <*> v2
>   v1 - v2 = pure (+) <*> v1 <*> v2
>   (*) = liftA2 (*)
>   negate = fmap negate
>   abs = fmap abs
>   signum = fmap signum
>   fromInteger i = error "fromInteger: Vector4 requires 4 components"

>instance Unfoldable Vector4 where
>   unfoldF f = f >>= \a -> f >>= \b -> f >>= \c -> f >>= \d ->
>     return $! Vector4 a b c d

>instance Comonad Vector4 where
>   extract (Vector4 t _ _ _) = t
>   duplicate (Vector4 t x y z) = Vector4 (Vector4 t x y z)
>                                         (Vector4 x y z t)
>                                         (Vector4 y z t x)
>                                         (Vector4 z t x y)

>x4 :: (Num a) => Vector4 a
>x4 = xcoord4 (cells identity4)
>y4 :: (Num a) => Vector4 a
>y4 = ycoord4 (cells identity4)
>z4 :: (Num a) => Vector4 a
>z4 = zcoord4 (cells identity4)
>t4 :: (Num a) => Vector4 a
>t4 = tcoord4 (cells identity4)
>dx4 = epsilon_closure %* x4
>dy4 = epsilon_closure %* y4
>dz4 = epsilon_closure %* z4
>dt4 = epsilon_closure %* t4

>type Matrix4 a = (Vector4 :*: Vector4) a

>codiag4 :: (Vector4 :*: Vector4) a -> Codiagonal Vector4 a
>codiag4 (Matrix m) = Codiagonal4 down right rest
>   where right = removex4 (xcoord4 m)
>         down = fmap xcoord4 (removex4 m)
>         rest = codiag3 $ Matrix $ fmap removex4 (removex4 m)

>matrix4 :: Vector4 a -> Codiagonal Vector4 a -> (Vector4 :*: Vector4) a
>matrix4 (Vector4 d1 d2 d3 d4) (Codiagonal4 down (Vector3 r1 r2 r3) rest) =
>       Matrix $ Vector4 (Vector4 d1 r1 r2 r3) v2 v3 v4
>    where Vector3 v2 v3 v4 = liftA2 (\i (Vector3 a b c) -> Vector4 i a b c) down (cells rest')
>          rest' = matrix3 (Vector3 d2 d3 d4) rest

>instance CodiagonalMatrix Vector4 a where
>   data Codiagonal Vector4 a = Codiagonal4 {
>      down_codiagonal4 :: Vector3 a,
>      right_codiagonal4 :: Vector3 a,
>      diagonal_codiagonal4 :: Codiagonal Vector3 a
>   }
>   type (Vector4 \\ a) = Vector3 a
>   codiagonal_impl = codiag4
>   (|\|) = matrix4
>   down_project = down_codiagonal4
>   right_project = right_codiagonal4

>instance Functor (Codiagonal Vector4) where
>   fmap f (Codiagonal4 down right diag) =
>     Codiagonal4 (fmap f down) (fmap f right) (fmap f diag)

>instance Applicative (Codiagonal Vector4) where
>   pure x = Codiagonal4 (pure x) (pure x) (pure x)
>   (Codiagonal4 f1 f2 fr) <*> (Codiagonal4 x1 x2 xr) =
>      Codiagonal4 (f1 <*> x1) (f2 <*> x2) (fr <*> xr)

>deriving instance (Show a) => Show (Codiagonal Vector4 a)

>instance (Num a) => StandardBasis (Vector4 a) where
>  unit_vectors = [Vector4 1 0 0 0,
>                  Vector4 0 1 0 0,
>                  Vector4 0 0 1 0,
>                  Vector4 0 0 0 1]                   

>instance (Num a) => Indexable Vector4 a where
>  diagonal_projections = diagonal_projections4
>  indexable_indices = Vector4 0 1 2 3

>diagonal_projections4 :: (Num a) => Vector4 (Index Vector4 a)
>diagonal_projections4 = Vector4 (tcoord4 <!-!> \a -> Vector4 a 0 0 0)
>                                (xcoord4 <!-!> \a -> Vector4 0 a 0 0)
>                                (ycoord4 <!-!> \a -> Vector4 0 0 a 0)
>                                (zcoord4 <!-!> \a -> Vector4 0 0 0 a)

>cycle4 :: Vector4 a -> Stream a
>cycle4 z@(Vector4 a b c d) = a `Pre` b `Pre` c `Pre` d `Pre` cycle4 z

>instance (Num a) => CoordinateSpace (Vector4 a) where
>  type Coordinate (Vector4 a) = Int
>  index = index4
>  listVector = vector4
>  dimension_size _ = 4
>  coordinates _ = [0,1,2,3]

>vector4 :: [a] -> Vector4 a
>vector4 [x,y,z,t] = Vector4 x y z t

>unzipV4 :: Vector4 (a,b) -> (Vector4 a, Vector4 b)
>unzipV4 (Vector4 (a1,b1) (a2,b2) (a3,b3) (a4,b4)) 
>  = (Vector4 a1 a2 a3 a4,Vector4 b1 b2 b3 b4)

>instance AppendableVector Vector1 Vector3 where
>  type (Vector1 :+: Vector3) = Vector4
>  (Vector1 x) |> (Vector3 y z t) = Vector4 x y z t

>instance SplittableVector Vector1 Vector3 where
>  vsplit (Vector4 x y z t) = (Vector1 x,Vector3 y z t)

>instance AppendableVector Vector3 Vector1 where
>  type (Vector3 :+: Vector1) = Vector4
>  (Vector3 x y z) |> (Vector1 t) = Vector4 x y z t

>instance SplittableVector Vector3 Vector1 where
>  vsplit (Vector4 x y z t) = (Vector3 x y z,Vector1 t)

>instance AppendableVector Vector2 Vector2 where
>  type (Vector2 :+: Vector2) = Vector4
>  (Vector2 x y) |> (Vector2 z t) = Vector4 x y z t

>instance (StreamBuilder str) => AppendableVector Vector4 str where
>  type (Vector4 :+: str) = str
>  (Vector4 x y z a) |> s = x `pre` y `pre` z `pre` a `pre` s

>instance SplittableVector Vector2 Vector2 where
>  vsplit (Vector4 x y z t) = (Vector2 x y, Vector2 z t)

instance NumSpace (Vector4 (Complex R))
instance FractionalSpace (Vector4 (Complex R))

>instance (Ord a) => Ord (Vector4 a) where
>  (Vector4 x y z t) <= (Vector4 x' y' z' t') = x <= x' && y <= y' && z <= z' && t <= t'

>instance (MedianAlgebra s) => MedianAlgebra (Vector4 s) where
>  med (Vector4 a b c t) (Vector4 a' b' c' t') (Vector4 a'' b'' c'' t'')
>   = Vector4 (med a a' a'') (med b b' b'') (med c c' c'') (med t t' t'')

>instance (PpShow a) => PpShow (Vector4 a) where
>  pp (Vector4 t x y z) = vcat $ zipWith nest [0,20,40,60] (map pp [t,x,y,z])

>instance (PpShow (f a)) => PpShow ((Vector4 :*: f) a) where
>  pp (Matrix (Vector4 t x y z)) = verticalize $ map pp [t,x,y,z]


>instance (Show a) => Show (Vector4 a) where
>  show (Vector4 t x y z) = "[" ++ show t ++ "," ++ show x ++ "," ++ show y ++ "," ++ show z ++ "]"

>instance (Read a) => Read (Vector4 a) where
>  readsPrec i str = do pre <- char '[' str
>                       (x,xr) <- readsPrec 0 pre
>                       xr_ <- char ',' xr
>                       (y,yr) <- readsPrec 0 xr_
>                       yr_ <- char ',' yr
>                       (z,zr) <- readsPrec 0 yr_
>                       zr_ <- char ',' zr
>                       (t,tr) <- readsPrec 0 zr_
>                       zt_ <- char ']' tr
>                       return $! (Vector4 x y z t,zt_)
>   where char ch (ch2:cr) | ch == ch2 = return cr
>                          | otherwise = []
>         char ch [] = []


>instance (Show (f a)) => Show ((Vector4 :*: f) a) where
>  show (Matrix (Vector4 a b c d)) = show a ++ "\n" ++ show b ++ "\n" ++ show c ++ "\n" ++ show d

>instance Functor Vector4 where
>  fmap f (Vector4 t x y z) = Vector4 (f t) (f x) (f y) (f z) 

>instance Monad Vector4 where
>  return x = Vector4 x x x x
>  x >>= f = diagonal4 $ Matrix $ f <$> x

>constant4 :: a -> Vector4 a
>constant4 = return

>instance Visitor (Vector4 a) where
>  data Fold (Vector4 a) b = Vector4Fold (a -> a -> a -> a -> b)
>  visit (Vector4Fold f) (Vector4 x y z t) = f x y z t

>instance IdVisitor (Vector4 a) where
>   idFold = Vector4Fold Vector4 

>instance Foldable Vector4 where
>   foldMap f (Vector4 t x y z) = f t `mappend` f x `mappend` f y `mappend` f z

>instance Traversable Vector4 where
>   traverse f (Vector4 t x y z) = Vector4 <$> f t <*> f x <*> f y <*> f z

>instance (Universe a) => Universe (Vector4 a) where
>   all_elements = Vector4 <$> all_elements
>                          <*> all_elements
>                          <*> all_elements
>                          <*> all_elements

>instance (Num s) => Semigroup (Vector4 s) where
>   (<>) = (%+)

>instance (Num s) => Monoid (Vector4 s) where
>  mempty = vzero
>  mappend = (%+)

>instance (Num a, ConjugateSymmetric a) => Semigroup ((Vector4 :*: Vector4) a) where
>   (<>) = (%*%)

>dim4 :: Vector4 Integer
>dim4 = Vector4 0 1 2 3

>-- | see "Lawvere,Rosebrugh: Sets for mathematics", pg. 167.
>instance (Num a, ConjugateSymmetric a) => Monoid ((Vector4 :*: Vector4) a) where
>   mempty = identity
>   mappend = (%*%)

>instance (Monad str, Limiting str a) => Limiting str (Vector4 a) where
>  data Closure str (Vector4 a) = Vector4Closure { runVector4Closure :: Vector4 (Closure str a) }
>  limit str = Vector4Closure $ Vector4
>                      (limit $ str >>= (return . tcoord4))
>                      (limit $ str >>= (return . xcoord4))
>                      (limit $ str >>= (return . ycoord4))
>                      (limit $ str >>= (return . zcoord4))
>  approximations (Vector4Closure (Vector4 a b c t)) = do
>     (a',b',c',t') <- fzip4 (approximations a) (approximations b) (approximations c) (approximations t)
>     return $! Vector4 a' b' c' t'

>sum_coordinates4 :: (Num a) => Vector4 a -> a
>sum_coordinates4 (Vector4 a b c d) = a + b + c + d

>instance Applicative Vector4 where
>  pure x = Vector4 x x x x
>  (Vector4 f g h i) <*> (Vector4 x y z t) = Vector4 (f x) (g y) (h z) (i t)

>instance (Num a) => VectorSpace (Vector4 a) where
>  type Scalar (Vector4 a) = a
>  vzero = Vector4 0 0 0 0
>  vnegate x = negate <$> x
>  v %* w = (v *) <$> w
>  x %+ y = pure (+) <*> x <*> y

>left_multiply4_gen :: (Functor f, Num a, ConjugateSymmetric a) => Vector4 a -> (f :*: Vector4) a -> f a
>left_multiply4_gen v (Matrix w) = (sum_coordinates4 . (pure (*) <*> v <*>)) <$> fmap conj w

>right_multiply4_gen :: (VectorSpace (f a), ConjugateSymmetric a, Scalar (f a) ~ a) => (Vector4 :*: f) a -> Vector4 a -> f a
>right_multiply4_gen (Matrix w) v = vsum $ liftA2 (\a fa -> a %* fa) (conj v) w

>matrix_multiply4 :: (Num a, ConjugateSymmetric a) => Matrix4 a -> Matrix4 a -> Matrix4 a
>matrix_multiply4 (Matrix v) w | Matrix wt <- transpose4_impl w = Matrix $ functor_outer dot4 v wt

>dot4 :: (Num a, ConjugateSymmetric a) => Vector4 a -> Vector4 a -> a
>dot4 x y = sum_coordinates4 $ pure (*) <*> x <*> conj y

>instance {-# OVERLAPPABLE #-} (Num a, ConjugateSymmetric a) => LinearTransform Vector4 Vector4 a where
>   (<*>>) = left_multiply4_gen
>   (<<*>) = right_multiply4_gen

>instance (Num a, ConjugateSymmetric a) => LinearTransform Vector3 Vector4 a where
>   (<*>>) = left_multiply4_gen
>   (<<*>) = right_multiply3_gen
>
>instance (Num a, ConjugateSymmetric a) => LinearTransform Vector4 Vector3 a where
>   (<*>>) = left_multiply3_gen
>   (<<*>) = right_multiply4_gen
>
>instance (Num a, ConjugateSymmetric a) => LinearTransform Vector2 Vector4 a where
>   (<*>>) = left_multiply4_gen
>   (<<*>) = right_multiply2_gen

>instance (Num a, ConjugateSymmetric a) => LinearTransform Vector4 Vector2 a where
>   (<*>>) = left_multiply2_gen
>   (<<*>) = right_multiply4_gen


>instance (Num a, ConjugateSymmetric a) => LinearTransform Vector1 Vector4 a where
>   x <*>> (Matrix (Vector1 m)) = Vector1 $ x %. m
>   (Matrix (Vector1 m)) <<*> (Vector1 x) = conj x %* m

>instance (Num a, ConjugateSymmetric a) => LinearTransform Vector4 Vector1 a where
>   (Vector1 x) <*>> (Matrix m) = x %* fmap (conj . vector_element) m
>   (Matrix (Vector4 (Vector1 t) (Vector1 x) (Vector1 y) (Vector1 z))) <<*> (Vector4 t' x' y' z')
>      = Vector1 (t*conj t'+x*conj x'+y*conj y'+z*conj z')


>instance (Floating a, ConjugateSymmetric a) => NormedSpace (Vector4 a) where
>  norm v = sqrt (v %. v)

>instance {-# OVERLAPPABLE #-} (Num a, ConjugateSymmetric a) => InnerProductSpace (Vector4 a) where
>  v %. w = sum_coordinates4 $ pure (*) <*> v <*> (conj <$> w)

>versor :: (Floating a, ConjugateSymmetric a) => Vector4 a -> Vector4 a
>versor q = (1 / norm q) %* q

>instance (Num a, ConjugateSymmetric a) => Num ((Vector4 :*: Vector4) a) where
>   (Matrix v) + (Matrix v') = Matrix $ liftA2 (liftA2 (+)) v v'
>   (Matrix v) - (Matrix v') = Matrix $ liftA2 (liftA2 (-)) v v'
>   (*) = (%*%)
>   negate (Matrix v) = Matrix $ liftA (liftA negate) v
>   abs (Matrix v) = Matrix $ liftA (liftA abs) v
>   signum (Matrix v) = Matrix $ liftA (liftA signum) v
>   fromInteger i = diagonal_matrix_impl (constant4 (fromInteger i))

>index4 :: Int -> Vector4 a -> a
>index4 0 = tcoord4
>index4 1 = xcoord4
>index4 2 = ycoord4
>index4 3 = zcoord4

>diagonal4 :: (Vector4 :*: Vector4) a -> Vector4 a
>diagonal4 (Matrix (Vector4 t x y z))
>   = Vector4 (tcoord4 t) (xcoord4 x) (ycoord4 y) (zcoord4 z)

>dt_4 eps = \case { (Vector4 t x y z) -> Vector4 (t+eps) x y z }
>dx_4 eps = \case { (Vector4 t x y z) -> Vector4 t (x+eps) y z }
>dy_4 eps = \case { (Vector4 t x y z) -> Vector4 t x (y+eps) z }
>dz_4 eps = \case { (Vector4 t x y z) -> Vector4 t x y (z+eps) }


>-- | 1 x 4 matrices:
>instance (Num a) => VectorSpace ((Vector1 :*: Vector4) a) where
>  type Scalar ((Vector1 :*: Vector4) a) = a
>  vzero = Matrix $ Vector1 vzero 
>  vnegate (Matrix v) = Matrix $ fmap (fmap negate) v
>  v %* (Matrix x) = Matrix $ fmap (fmap (v *)) x
>  (Matrix x) %+ (Matrix y) = Matrix $ liftA2 (liftA2 (+)) x y

>-- | 1 x 4 matrices:
>instance Transposable Vector1 Vector4 a where
>   transpose_impl (Matrix (Vector1 (Vector4 t x y z)))
>     = Matrix $ Vector4 (Vector1 t) (Vector1 x) (Vector1 y) (Vector1 z)

>-- | 4 x 1 matrices:
>instance Transposable Vector4 Vector1 a where
>   transpose_impl (Matrix (Vector4 (Vector1 t) (Vector1 x) (Vector1 y) (Vector1 z)))
>     = Matrix $ Vector1 (Vector4 t x y z)

>instance Transposable Stream Vector4 a where
>   transpose_impl (Matrix (Pre v vr))
>     = Matrix (Vector4 (Pre (tcoord4 v) (fmap tcoord4 vr))
>                       (Pre (xcoord4 v) (fmap xcoord4 vr))
>                       (Pre (ycoord4 v) (fmap ycoord4 vr))
>                       (Pre (zcoord4 v) (fmap zcoord4 vr)))

>-- | 4 x 1 matrices:
>instance (Num a) => VectorSpace ((Vector4 :*: Vector1) a) where
>  type Scalar ((Vector4 :*: Vector1) a) = a
>  vzero = Matrix $ Vector4 vzero vzero vzero vzero
>  vnegate (Matrix v) = Matrix $ fmap negate v
>  v %* (Matrix x) = Matrix $ fmap (fmap (v *)) x
>  (Matrix x) %+ (Matrix y) = Matrix $ x %+ y

>-- | 2 x 4 matrices:
>instance Transposable Vector2 Vector4 a where
>    transpose_impl (Matrix (Vector2 (Vector4 x1 x2 x3 x4) (Vector4 y1 y2 y3 y4)))
>      = Matrix $ Vector4 (Vector2 x1 y1) (Vector2 x2 y2) (Vector2 x3 y3) (Vector2 x4 y4)

>-- | 2 x 4 matrices
>instance (Num a) => VectorSpace ((Vector2 :*: Vector4) a) where
>  type Scalar ((Vector2 :*: Vector4) a) = a
>  vzero = Matrix $ Vector2 vzero vzero
>  vnegate (Matrix v) = Matrix $ fmap (fmap negate) v
>  v %* (Matrix x) = Matrix $ fmap (fmap (v *)) x
>  (Matrix x) %+ (Matrix y) = Matrix $ liftA2 (liftA2 (+)) x y

>-- | 4 x 2 matrices
>instance Transposable Vector4 Vector2 a where
>    transpose_impl (Matrix (Vector4 (Vector2 x1 y1) (Vector2 x2 y2) (Vector2 x3 y3) (Vector2 x4 y4)))
>       = Matrix $ Vector2 (Vector4 x1 x2 x3 x4) (Vector4 y1 y2 y3 y4)

>-- | 4 x 2 matrices
>instance (Num a) => VectorSpace ((Vector4 :*: Vector2) a) where
>  type Scalar ((Vector4 :*: Vector2) a) = a
>  vzero = Matrix $ Vector4 vzero vzero vzero vzero
>  vnegate (Matrix v) = Matrix $ fmap (fmap negate) v
>  v %* (Matrix x) = Matrix $ fmap (fmap (v *)) x
>  (Matrix x) %+ (Matrix y) = Matrix $ x %+ y

>-- | 4 x 3 matrices:
>instance Transposable Vector4 Vector3 a where
>   transpose_impl (Matrix (Vector4 (Vector3 x1 y1 z1) 
>                              (Vector3 x2 y2 z2)
>                              (Vector3 x3 y3 z3)
>                              (Vector3 x4 y4 z4)))
>    = Matrix $ Vector3 (Vector4 x1 x2 x3 x4)
>                       (Vector4 y1 y2 y3 y4)
>                       (Vector4 z1 z2 z3 z4)

>-- | 4 x 3 matrices:
>instance (Num a) => VectorSpace ((Vector4 :*: Vector3) a) where
>  type Scalar ((Vector4 :*: Vector3) a) = a
>  vzero = Matrix $ Vector4 vzero vzero vzero vzero
>  vnegate (Matrix v) = Matrix $ fmap (fmap negate) v
>  v %* (Matrix x) = Matrix $ fmap (fmap (v *)) x
>  (Matrix x) %+ (Matrix y) = Matrix $ x %+ y

>-- | 3 x 4 matrices
>instance Transposable Vector3 Vector4 a where
>   transpose_impl (Matrix (Vector3 (Vector4 x1 x2 x3 x4)
>                              (Vector4 y1 y2 y3 y4)
>                              (Vector4 z1 z2 z3 z4)))
>     = Matrix $ Vector4 (Vector3 x1 y1 z1)
>                        (Vector3 x2 y2 z2)
>                        (Vector3 x3 y3 z3)
>                        (Vector3 x4 y4 z4)

>-- | 3 x 4 matrices
>instance (Num a) => VectorSpace ((Vector3 :*: Vector4) a) where
>  type Scalar ((Vector3 :*: Vector4) a) = a
>  vzero = Matrix $ Vector3 vzero vzero vzero
>  vnegate (Matrix v) = Matrix $ fmap (fmap negate) v
>  v %* (Matrix x) = Matrix $ fmap (fmap (v *)) x
>  (Matrix x) %+ (Matrix y) = Matrix $ liftA2 (liftA2 (+)) x y

>-- | 4 x 4 matrices
>instance Transposable Vector4 Vector4 a where
>   transpose_impl = transpose4_impl

>instance Transposable Vector4 ((->) row) a where
>   transpose_impl (Matrix (Vector4 t x y z))
>     = Matrix $ \a -> Vector4 (t a) (x a) (y a) (z a)
>instance Transposable ((->) row) Vector4 a where
>   transpose_impl (Matrix f)
>     = Matrix $ Vector4 (\a -> tcoord4 (f a))
>                        (\a -> xcoord4 (f a))
>                        (\a -> ycoord4 (f a))
>                        (\a -> zcoord4 (f a))

>-- | 4 x 4 matrices
>instance (Num a) => VectorSpace ((Vector4 :*: Vector4) a) where
>  type Scalar ((Vector4 :*: Vector4) a) = a
>  vzero = Matrix $ Vector4 vzero vzero vzero vzero
>  vnegate (Matrix v) = Matrix $ fmap (fmap negate) v
>  v %* (Matrix x) = Matrix $ fmap (fmap (v *)) x
>  (Matrix x) %+ (Matrix y) = Matrix $ liftA2 (liftA2 (+)) x y

>instance (ConjugateSymmetric a) => ConjugateSymmetric (Vector4 a) where
>   conj = fmap conj

>-- | <https://en.wikipedia.org/wiki/Conjugate_transpose>
>instance (ConjugateSymmetric a) => ConjugateSymmetric ((Vector4 :*: Vector4) a) where
>   conj = fmap conj . transpose4_impl

>instance Control.Monad.Zip.MonadZip Vector4 where
>  mzip ~(Vector4 t x y z) ~(Vector4 t' x' y' z') = Vector4 (t,t') (x,x') (y,y') (z,z')
>  mzipWith f ~(Vector4 t x y z) ~(Vector4 t' x' y' z') = Vector4 (f t t') (f x x') (f y y') (f z z')
>  munzip ~(Vector4 ~(t,t') ~(x,x') ~(y,y') ~(z,z')) = (Vector4 t x y z, Vector4 t' x' y' z')


>transpose4 :: (Num a) => Matrix4 a -> Matrix4 a
>transpose4 = indexable_transpose

>transpose4_impl :: Matrix4 a -> Matrix4 a
>transpose4_impl ~(Matrix ~(Vector4 ~(Vector4 x1 x2 x3 x4)
>                                 ~(Vector4 y1 y2 y3 y4)
>                                 ~(Vector4 z1 z2 z3 z4)
>                                 ~(Vector4 t1 t2 t3 t4)))
>   = Matrix $ Vector4 (Vector4 x1 y1 z1 t1)
>                      (Vector4 x2 y2 z2 t2)
>                      (Vector4 x3 y3 z3 t3)
>                      (Vector4 x3 y4 z4 t4)

>-- | 4 x 4 matrices
>instance (Floating a, ConjugateSymmetric a) => NormedSpace ((Vector4 :*: Vector4) a) where
>  norm = innerproductspace_norm

>-- | 4 x 4 matrices
>instance (Floating a, ConjugateSymmetric a) => InnerProductSpace ((Vector4 :*: Vector4) a) where
>  x %. y = trace_impl (transpose_impl x %*% y)

>instance (Num a) => Diagonalizable Vector4 a where
>  vector_dimension _ = dim4
>  identity_impl _ = identity4
>  identity = identity4
>  diagonal_impl = diagonal4
>  diagonal_matrix_impl = diagonal_matrix4

>instance (Num a) => Traceable Vector4 a where
>  determinant_impl = determinant4
>  trace_impl    = trace4

>identity4 :: (Num a) => Matrix4 a
>identity4 = diagonal_matrix_impl (constant4 1)

>diagonal4x :: Matrix4 a -> Vector4 a
>diagonal4x (Matrix (Vector4 t x y z)) =
>   Vector4 (tcoord4 t) (xcoord4 x) (ycoord4 y) (zcoord4 z)

>zero_codiagonal4 :: (Num a) => Codiagonal Vector4 a
>zero_codiagonal4 = Codiagonal4 (constant3 0) (constant3 0) (zero_codiagonal3)

>diagonal_matrix4 :: (Num a) => Vector4 a -> (Vector4 :*: Vector4) a
>diagonal_matrix4 v = matrix4 v zero_codiagonal4

>mat4 :: Vector4 a -> Codiagonal Vector4 a -> (Vector4 :*: Vector4) a
>mat4 (Vector4 d1 d2 d3 d4) (Codiagonal4 (Vector3 a' a'' a''') (Vector3 a b c)
>                             (Codiagonal3 (Vector2 x'' x''') (Vector2 b' c')
>                               (Codiagonal2 (Vector1 b''') (Vector1 c'')))) = Matrix $
>   Vector4 (Vector4 d1   a    b   c)
>           (Vector4 a'  d2    b'  c')
>           (Vector4 a''  x''  d3  c'')
>           (Vector4 a''' x''' b''' d4)

>trace4 :: (Num a) => Matrix4 a -> a
>trace4 m | Vector4 t x y z <- diagonal4x m = x + y + z + t

>removet4 :: Vector4 a -> Vector3 a
>removet4 (Vector4 t x y z) = Vector3 x y z

>removex4 :: Vector4 a -> Vector3 a
>removex4 (Vector4 t x y z) = Vector3 t y z

>removey4 :: Vector4 a -> Vector3 a
>removey4 (Vector4 t x y z) = Vector3 t x z

>removez4 :: Vector4 a -> Vector3 a
>removez4 (Vector4 t x y z) = Vector3 t x y

>removes4 :: Vector4 (Vector4 a -> Vector3 a)
>removes4 = Vector4 removet4 removex4 removey4 removez4

>remove_index4 :: Matrix4 a -> Matrix4 (Matrix3 a)
>remove_index4 (Matrix m) = matrix app removes4 removes4
>   where app f g = Matrix $ f $ fmap g m
>
>cofactor4 :: (Num a) => Matrix4 a -> Matrix4 a
>cofactor4 m = pure (*) <*> fmap fromIntegral signs4 <*> fmap determinant_impl (remove_index4 m)
>
>adjucate4 :: (Num a) => Matrix4 a -> Matrix4 a
>adjucate4 = transpose_impl . cofactor4

>signs4 :: (Integral a) => Matrix4 a
>signs4 = fmap (\ (i,j) -> ((i+j+1) `mod` 2) * 2 - 1) matrix_indices4

>vector_indices4 :: (Integral a) => Vector4 a
>vector_indices4 = Vector4 0 1 2 3

>matrix_indices4 :: (Integral a) => (Vector4 :*: Vector4) (a,a)
>matrix_indices4 = matrix (,) vector_indices4 vector_indices4

>-- | <https://en.wikipedia.org/wiki/Invertible_matrix>
>inverse4 :: (Fractional a) => Matrix4 a -> Matrix4 a
>inverse4 m = (1 / determinant_impl m) %* adjucate4 m

>instance (Fractional a, ConjugateSymmetric a) => Group ((Vector4 :*: Vector4) a) where
>   ginvert = inverse4

>-- | Generalization of cross product to four dimensions
>-- This is computed using formal determinant representation of cross product
>-- expanded to four dimensions
>-- <https://en.wikipedia.org/wiki/Cross_product>
>-- This computes vector that is linearly independent of all three vectors given
>-- in four dimensional space.
>cross4 :: (Num a) => Vector4 a -> Vector4 a -> Vector4 a -> Vector4 a
>cross4 v1 v2 v3 = Vector4 det1 det2 det3 det4
>  where det1 =          determinant3 $ Matrix $ fmap removet4 v
>        det2 = negate $ determinant3 $ Matrix $ fmap (rotate . removex4) v
>        det3 =          determinant3 $ Matrix $ fmap (inverseRotate . removey4) v
>        det4 = negate $ determinant3 $ Matrix $ fmap removez4 v
>        v = Vector3 v1 v2 v3

>cross_product4 :: (Num a) => (Vector3 :*: Vector4) a -> Vector4 a
>cross_product4 (Matrix (Vector3 a b c)) = cross4 a b c

>determinant4 :: (Num a) => Matrix4 a -> a
>determinant4 (Matrix m) = combine $ pure (*) <*> tcoord4 m <*> amv  
>   where amv = fmap (determinant_impl . Matrix . removet4 . (`fmap` m)) removes4
>         combine (Vector4 a b c d) = a - b + c - d

>instance (Fractional a) => Invertible Vector4 a where
>   inverse_impl = inverse4
>   adjucate_impl = adjucate4
>   cofactor_impl = cofactor4

