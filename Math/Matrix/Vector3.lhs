>{-# LANGUAGE Safe,MultiParamTypeClasses, TypeSynonymInstances #-}
>{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeOperators #-}
>{-# LANGUAGE TypeFamilies, PatternGuards, DataKinds, LambdaCase #-}
>{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveDataTypeable #-}
>{-# LANGUAGE PatternSynonyms, UndecidableInstances #-}
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
>import safe qualified Control.Monad.Zip
>import safe Math.Tools.Universe
>import safe Math.Tools.Functor
>import safe Math.Number.Interface
>import safe Math.Number.StreamInterface
>import safe Math.Matrix.Interface
>import safe Math.Tools.CoMonad
>import safe Math.Tools.Median
>import safe Math.Tools.NaturalTransformation
>import safe Math.Tools.Isomorphism
>import safe Math.Tools.Orthogonal hiding (outer3)
>import safe Math.Tools.Visitor
>import safe Math.Tools.PrettyP
>import safe Math.Tools.I
>import safe Math.Tools.Arrow
>import safe Math.Number.Group
>import safe Math.Matrix.Vector1
>import safe Math.Matrix.Vector2
>import safe Math.Matrix.Indexable
>import safe Math.Matrix.FiniteVector
>import safe qualified Math.Number.StreamInterface as Stream
>import safe Math.Number.StreamInterface (Limiting(..), Infinitesimal(..), Closed(..))

>-- | Three element vector
>data Vector3 s = Vector3 { xcoord3 :: !s, ycoord3 :: !s,  zcoord3 :: !s }

>deriving instance (Eq a) => Eq (Vector3 a)
>deriving instance (Typeable a) => Typeable (Vector3 a)
>deriving instance (Data a) => Data (Vector3 a)
>deriving instance (Generic a) => Generic (Vector3 a)

>instance (MetricSpace a, Floating (Distance a)) => MetricSpace (Vector3 a) where
>   type Distance (Vector3 a) = Distance a
>   distance (Vector3 x y z) (Vector3 x' y' z') =
>     sqrt ((distance x x')^2 + (distance y y')^2 + (distance z z')^2)

>instance (Binary s) => Binary (Vector3 s) where
>   put (Vector3 x y z) = put x >> put y >> put z
>   get = do { x <- get ; y <- get ; z <- get ; return $! (Vector3 x y z) }

>type ComplexVector3 a = (Vector3 :*: Complex) a

>instance Unfoldable Vector3 where
>   unfoldF f = f >>= \a -> f >>= \b -> f >>= \c -> return $! Vector3 a b c

>dim3 :: Vector3 Integer
>dim3 = Vector3 0 1 2

>i3 :: (Num a) => Vector3 a
>i3 = identity <!> (xcoord3,id)

>j3 :: (Num a) => Vector3 a
>j3 = identity <!> (ycoord3,id)
> 
>k3 :: (Num a) => Vector3 a
>k3 = identity <!> (zcoord3,id)

>instance Transposable Stream Vector3 a where
>   transposeImpl (Matrix (Pre v vr))
>     = Matrix (Vector3 (Pre (xcoord3 v) (fmap xcoord3 vr))
>                       (Pre (ycoord3 v) (fmap ycoord3 vr))
>                       (Pre (zcoord3 v) (fmap zcoord3 vr)))


>instance CircularComonad Vector3 where
>   rotate (Vector3 x y z) = Vector3 y z x
>   
>instance FiniteComonad Vector3 where
>   inverseRotate (Vector3 x y z) = Vector3 z x y

>instance (Universe a) => Universe (Vector3 a) where
>   allElements = Vector3 <$> allElements <*> allElements <*> allElements

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



>type Matrix3 a = (Vector3 :*: Vector3) a


>vec3 :: (a,a,a) -> Vector3 a
>vec3 (x,y,z) = Vector3 x y z 

>finite3 :: Vector3 a -> Vec 3 a
>finite3 (Vector3 x y z) = Cons x (Cons y (Cons z Empty))


>constant3 :: a -> Vector3 a
>constant3 x = Vector3 x x x

>vec3Unit :: a -> Scalar (Vector3 a)
>vec3Unit x = x

>vec3Counit :: Vector3 (Scalar (Vector3 a)) -> Vector3 a
>vec3Counit (Vector3 x y z) = Vector3 x y z

>destructvec3 :: Vector3 a -> (a,a,a)
>destructvec3 (Vector3 x y z) = (x,y,z)

>-- lambda used to ensure inlining.
>setx3 :: s -> Vector3 s -> Vector3 s
>setx3 x = \v -> v { xcoord3 = x }

>sety3 :: s -> Vector3 s -> Vector3 s
>sety3 y = \v -> v { ycoord3 = y }

>setz3 :: s -> Vector3 s -> Vector3 s
>setz3 z = \v -> v { zcoord3 = z }

>setEndo :: Vector3 s -> Vector3 (Mon.Endo (Vector3 s))
>setEndo = fmap Mon.Endo . setVectorAction

>setVectorAction :: Vector3 s -> Vector3 (Vector3 s -> Vector3 s)
>setVectorAction (Vector3 x y z) = Vector3 (setx3 x) (sety3 y) (setz3 z)

>-- | update_column3 v is a vector of all update operations that replace one row with 'v'
>updateRow3 :: g a -> Vector3 ((Vector3 :*: g) a -> (Vector3 :*: g) a)
>updateRow3 x = Vector3 (updateRow setx3 x) (updateRow sety3 x) (updateRow setz3 x)

>-- | update_column3 v is a vector of all update operations that replace one column with 'v'
>-- example use:
>-- update_column3 (Vector3 3 4 5) `ycoord3` identity3 == [[1,3,0],[0,4,0],[0,5,1]].
>updateColumn3 :: (Applicative f) => f a -> Vector3 ((f :*: Vector3) a -> (f :*: Vector3) a)
>updateColumn3 x = Vector3 (updateColumn setx3 x) (updateColumn sety3 x) (updateColumn setz3 x)

>instance UpdateableMatrixDimension Vector3 where
>  writeRow = updateRow3
>  writeColumn = updateColumn3

>removex3 :: Vector3 a -> Vector2 a
>removex3 (Vector3 _ y z) = Vector2 y z

>removey3 :: Vector3 a -> Vector2 a
>removey3 (Vector3 x _ z) = Vector2 x z

>removez3 :: Vector3 a -> Vector2 a
>removez3 (Vector3 x y _) = Vector2 x y

>remove3 :: Vector3 (Vector3 a -> Vector2 a)
>remove3 = Vector3 removex3 removey3 removez3

>determinancett3 :: (Num a) => Determinance Vector3 Vector3 Vector2 Vector2 Vector2 a a a
>determinancett3 = Determinance remove3 remove3 xcoord3 xcoord3 combine3 determinantImpl
>  where combine3 (Vector3 a b c) = a - b + c


>remove_index3 :: Matrix3 a -> Matrix3 (Matrix2 a)
>remove_index3 (Matrix m) = matrix app remove3 remove3
>   where app f g = Matrix $ f $ fmap g m

>cofactor3 :: (Num a) => Matrix3 a -> Matrix3 a
>cofactor3 m = pure (*) <*> fmap fromIntegral signs3 <*> fmap determinantImpl (remove_index3 m)

>-- | <https://en.wikipedia.org/wiki/Adjugate_matrix>

>adjucate3 :: (Num a) => Matrix3 a -> Matrix3 a
>adjucate3 = transposeImpl . cofactor3

>-- | <https://en.wikipedia.org/wiki/Invertible_matrix>

>inverse3 :: (Fractional a) => Matrix3 a -> Matrix3 a
>inverse3 m = (1 / determinantImpl m) %* adjucate3 m

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
>         -> ((Vector3 :*: Vector3) :*: Vector3) a
>         -> ((Vector3 :*: Vector3) :*: Vector3) a
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
>signs3 = fmap (\ (i,j) -> ((i+j+1) `mod` 2) * 2 - 1) matrixIndices3

>instance (Ord a) => Ord (Vector3 a) where
>  (Vector3 x y z) <= (Vector3 x' y' z') = x <= x' && y <= y' && z <= z'

>instance Visitor (Vector3 a) where
>  data Fold (Vector3 a) b = Vector3Fold (a -> a -> a -> b)
>  visit (Vector3Fold f) (Vector3 x y z) = f x y z

>instance AppendableVector Vector2 Vector1 where
>  type (Vector2 :+: Vector1) = Vector3
>  (Vector2 x y) ||>> (Vector1 z) = Vector3 x y z

>instance SplittableVector Vector2 Vector1 where
>  vsplit (Vector3 x y z) = (Vector2 x y,Vector1 z)

>instance AppendableVector Vector1 Vector2 where
>  type (Vector1 :+: Vector2) = Vector3
>  (Vector1 x) ||>> (Vector2 y z) = Vector3 x y z

>instance SplittableVector Vector1 Vector2 where
>  vsplit (Vector3 x y z) = (Vector1 x, Vector2 y z)

>instance Foldable Vector3 where
>   foldMap f (Vector3 x y z) = f x `mappend` f y `mappend` f z

>instance Traversable Vector3 where
>   traverse f (Vector3 x y z) = Vector3 <$> f x <*> f y <*> f z

>instance (ShowPrecision s) => ShowPrecision (Vector3 s) where
>   showAtPrecision (Vector3 x y z) p =
>     "(" ++ showAtPrecision x p ++ "," ++
>            showAtPrecision y p ++ "," ++
>            showAtPrecision z p ++ ")"

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
>   ppfVertical (Vector3 x y z) = pp '[' <> vcat [pp x,pp y, pp z] <> pp ']'
>instance PpShowF Vector3 where
>   ppf (Vector3 x y z) = pp '[' Mon.<> (sep [pp x,pp ',', pp y, pp ',',pp z]) Mon.<> pp ']'

instance (PpShow (f a)) => PpShow ((Vector3 :*: f) a) where
  pp (Matrix (Vector3 x y z)) = verticalize $ map pp [x,y,z]

>instance Functor Vector3 where
>  fmap f = \ (Vector3 x y z) -> Vector3 (f x) (f y) (f z)

>instance Monad Vector3 where
>  return = pure
>  x >>= f = diagonal3 $ Matrix $ fmap f x

>instance Control.Monad.Zip.MonadZip Vector3 where
>  mzip ~(Vector3 x y z) ~(Vector3 x' y' z') = Vector3 (x,x') (y,y') (z,z')
>  mzipWith f ~(Vector3 x y z) ~(Vector3 x' y' z') = Vector3 (f x x') (f y y') (f z z')
>  munzip (Vector3 ~(x,x') ~(y,y') ~(z,z')) = (Vector3 x y z, Vector3 x' y' z')

>instance (Num s) => Semigroup (Vector3 s) where
>   (<>) = (%+)

>instance (Num s) => Mon.Monoid (Vector3 s) where
>  mempty = vzero
>  
>-- | see "Lawvere,Rosebrugh: Sets for mathematics", pg. 167.
>instance (Ord a, ConjugateSymmetric a, Num a) => Semigroup ((Vector3 :*: Vector3) a) where
>   (<>) = (%*%)


>-- | see "Lawvere,Rosebrugh: Sets for mathematics", pg. 167.
>instance (Ord a, Num a, ConjugateSymmetric a) => Mon.Monoid ((Vector3 :*: Vector3) a) where
>   mempty  = identity

>instance (Num s) => Group (Vector3 s) where
>   ginvert (Vector3 x y z) = Vector3 (negate x) (negate y) (negate z)

>instance (Ord a, Fractional a, ConjugateSymmetric a) => Group ((Vector3 :*: Vector3) a) where
>   ginvert = inverseImpl


approximations_vector3 :: Vector3 R -> Stream (Vector3 R)
approximations_vector3 (Vector3 x y z) = do
        (x',(y',z')) <- approximate x `zip` (approximate y `zip` approximate z)
        return $ Vector3 (Limit x') (Limit y') (Limit z')

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


>op_dot :: (Num b) => Vector3 (a -> b) -> a -> b
>op_dot (Vector3 f g h) = \ z -> f z + g z + h z

>instance Applicative Vector3 where
>   pure x = Vector3 x x x
>   (Vector3 f g h) <*> (Vector3 x y z) = Vector3 (f x) (g y) (h z)

>applicative_matrix :: (Num b) => Vector3 (b -> b) -> (Vector3 :*: Vector3) b
>applicative_matrix v = functionMatrix (v <*>)

>instance (Num a) => VectorSpace (Vector3 a) where
>  type Scalar (Vector3 a) = a
>  vzero = pure 0
>  vnegate = liftA negate
>  v %* w = liftA (v*) w
>  (%+) = liftA2 (+)

>instance (ConjugateSymmetric a) => ConjugateSymmetric (Vector3 a) where
>   conj (Vector3 x y z) = Vector3 (conj x) (conj y) (conj z)

>-- | <https://en.wikipedia.org/wiki/Conjugate_transpose>
>instance (ConjugateSymmetric a, Num a) => ConjugateSymmetric ((Vector3 :*: Vector3) a) where
>   conj = fmap conj . transposeImpl

>instance (Ord a, Floating a, ConjugateSymmetric a) => NormedSpace (Vector3 a) where
>  norm = innerproductspaceNorm
>  normSquared = innerproductspaceNormSquared

>instance {-# OVERLAPPABLE #-} (ConjugateSymmetric a, Num a) => InnerProductSpace (Vector3 a) where
>  (Vector3 x y z) %. (Vector3 x' y' z') = x * conj x' + y * conj y' + z * conj z'

>{-# INLINABLE sum_coordinates3 #-}
>sum_coordinates3 :: (Num a) => Vector3 a -> a
>sum_coordinates3 (Vector3 x y z) = x + y + z

>-- | cube root of a sum of cubes.
>cube_norm3 :: (Floating a) => Vector3 a -> a
>cube_norm3 (Vector3 x y z) = let n = 3 in (x**n + y**n + z**n)**(1/n)

>-- | nth root of a sum of nth powers
>nth_norm3 :: (Floating a) => a -> Vector3 a -> a
>nth_norm3 n (Vector3 x y z) = (x**n + y**n + z**n)**(1/n)

>instance Diagonalizable Vector3 a where
>  identity = identity3
>  diagonalImpl = diagonal3
>  diagonalMatrixImpl = diagonal_matrix3

>instance (Num a) => Traceable Vector3 a where
>  traceImpl = trace3 
>  determinantImpl = determinant3 

>instance Transposable Vector3 Vector2 a where
>  transposeImpl (Matrix (Vector3 (Vector2 x1 y1) (Vector2 x2 y2) (Vector2 x3 y3)))
>    = Matrix $ Vector2 (Vector3 x1 x2 x3) (Vector3 y1 y2 y3)

>instance Transposable Vector2 Vector3 a where
>  transposeImpl (Matrix (Vector2 (Vector3 x1 x2 x3) (Vector3 y1 y2 y3)))
>    = Matrix $ Vector3 (Vector2 x1 y1) (Vector2 x2 y2) (Vector2 x3 y3)

>instance Transposable Vector3 Vector1 a where
>  transposeImpl (Matrix x) = Matrix $ Vector1 (fmap vectorElement x)

>instance Transposable Vector1 Vector3 a where
>  transposeImpl (Matrix (Vector1 x)) = Matrix $ fmap Vector1 x

>instance Transposable Vector3 Vector3 a where
>  transposeImpl = transpose3Impl

>instance Transposable Vector3 ((->) row) a where
>   transposeImpl (Matrix (Vector3 x y z))
>     = Matrix $ \a -> Vector3 (x a) (y a) (z a)
>instance Transposable ((->) row) Vector3 a where
>   transposeImpl (Matrix f)
>     = Matrix $ Vector3 (\a -> xcoord3 (f a))
>                        (\a -> ycoord3 (f a))
>                        (\a -> zcoord3 (f a))

>instance {-# INCOHERENT #-} (Num a, ConjugateSymmetric a) => LinearTransform Vector3 Vector3 a where
>  (<*>>) = leftMultiply3
>  (<<*>) = rightMultiply3

>instance (Num a) => CoordinateSpace (Vector3 a) where
>  type Coordinate (Vector3 a) = Int
>  index 0 = xcoord3
>  index 1 = ycoord3
>  index 2 = zcoord3
>  listVector = vector3
>  dimensionSize _ = 3
>  coordinates _ = [0,1,2]

>instance (Numeric a) => StandardBasis (Vector3 a) where
>  unitVectors = [xid,yid,zid]


>instance (Ord a, Numeric a, ConjugateSymmetric a) => Num ((Vector3 :*: Vector3) a) where
>  (Matrix v) + (Matrix v') = Matrix $ liftA2 (liftA2 (+)) v v'
>  (Matrix v) - (Matrix v') = Matrix $ liftA2 (liftA2 subtract) v v'
>  (*) = (%*%)
>  negate (Matrix v) = Matrix (negate v)
>  abs (Matrix v) = Matrix (abs v)
>  signum (Matrix v) = Matrix (signum v)
>  fromInteger x = diagonalMatrixImpl (constant3 $ fromInteger x) vzero

>instance (Ord a, NumFractional a, ConjugateSymmetric a) => Fractional ((Vector3 :*: Vector3) a) where
>   recip = inverseImpl
>   fromRational x = diagonalMatrixImpl (constant3 $ fromRational x) vzero


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

>mapBasis :: (Numeric a) => (Vector3 a -> Vector3 a) -> Vector3 a -> Vector3 a
>mapBasis f (Vector3 x y z) = x %* f iVec + y %* f jVec + z %* f kVec

>functionMatrix3 :: (Numeric a) => (Vector3 a -> Vector3 a) -> Matrix3 a
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


>xid,yid,zid :: (Num a) => Vector3 a
>xid = Vector3 1 0 0
>yid = Vector3 0 1 0
>zid = Vector3 0 0 1

>identity3 :: (Num a) => Matrix3 a
>identity3 = Matrix $ Vector3 xid yid zid


>matrixPower3 :: (Ord a, ConjugateSymmetric a,Numeric a) => Matrix3 a -> Integer -> Matrix3 a
>matrixPower3 mat = \case
>    0 -> identity3
>    i -> mat `matrixMultiply3` matrixPower3 mat (pred i)

>vectorIndices3 :: (Integral a) => Vector3 a
>vectorIndices3 = Vector3 1 2 3

>matrixIndices3 :: (Integral a) => (Vector3 :*: Vector3) (a,a)
>matrixIndices3 = matrix (,) vectorIndices3 vectorIndices3

>matrixMultiply3 :: (Ord a, ConjugateSymmetric a,Num a) => Matrix3 a -> Matrix3 a -> Matrix3 a
>matrixMultiply3 (Matrix m1) m2 
>   | Matrix m2t <- transpose3 m2 = matrix (%.) m1 m2t

>{-# INLINABLE dot3 #-}
>dot3 :: (Num a, ConjugateSymmetric a) => Vector3 a -> Vector3 a -> a
>dot3 x y = sum_coordinates3 $ liftA2 (*) x (fmap conj y)

>leftMultiply3Gen :: (Functor f, Num a, ConjugateSymmetric a) => Vector3 a -> (f :*: Vector3) a -> f a
>leftMultiply3Gen v (Matrix w) = (sum . (pure (*) <*> v <*>)) <$> fmap conj w

>rightMultiply3Gen :: (VectorSpace (f a), ConjugateSymmetric a, Scalar (f a) ~ a) => (Vector3 :*: f) a -> Vector3 a -> f a
>rightMultiply3Gen (Matrix w) v = vsum $ liftA2 (\a fa -> a %* fa) (conj v) w

>leftMultiply3 :: (Num a, ConjugateSymmetric a) => Vector3 a -> Matrix3 a -> Vector3 a
>leftMultiply3 v (Matrix m) = fmap (dot3 v) m

>rightMultiply3 :: (Num a, ConjugateSymmetric a) => Matrix3 a -> Vector3 a -> Vector3 a
>rightMultiply3 (Matrix m) v = fmap (dot3 v) m
             
>transpose3 :: (Num a) => Matrix3 a -> Matrix3 a
>transpose3 (Matrix m) = matrix runIndex diagonal_projections3 m

>{-# INLINABLE transpose3Impl #-}
>transpose3Impl :: Matrix3 a -> Matrix3 a
>transpose3Impl ~(Matrix ~(Vector3 ~(Vector3 x1 y1 z1)
>                                   ~(Vector3 x2 y2 z2)
>                                   ~(Vector3 x3 y3 z3)))
>   = Matrix $ Vector3 (Vector3 x1 x2 x3)
>                      (Vector3 y1 y2 y3)
>                      (Vector3 z1 z2 z3)

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

>diagonal_matrix3 :: Vector3 a -> (Vector3 :*: Vector3) a -> (Vector3 :*: Vector3) a
>diagonal_matrix3 v m = matrix3 v (codiag3 m)

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
>   projectFirst (Vector3 x _ _) = Vector1 x
>   projectSecond (Vector3 _ y z) = S31Vector $ Vector2 y z
>   joinVector (Vector1 x) (S31Vector (Vector2 y z)) = Vector3 x y z

>instance ProjectionSpace (Vector3 \\\ Vector1) Vector1 where
>   data ((Vector3 \\\ Vector1) \\\ Vector1) a = S311Vector (Vector1 a)
>   projectFirst (S31Vector (Vector2 x _)) = Vector1 x
>   projectSecond (S31Vector (Vector2 _ y)) = S311Vector (Vector1 y)
>   joinVector (Vector1 x) (S311Vector (Vector1 y)) = S31Vector $ Vector2 x y

>instance ProjectionSpace Vector3 Vector2 where
>   data (Vector3 \\\ Vector2) a = S32Vector (Vector1 a)
>   projectFirst (Vector3 x y _) = Vector2 x y
>   projectSecond (Vector3 _ _ z) = S32Vector $ Vector1 z
>   joinVector (Vector2 x y) (S32Vector (Vector1 z)) = Vector3 x y z

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
>      downCodiagonal3 :: Vector2 a,
>      rightCodiagonal3 :: Vector2 a,
>      diagonalCodiagonal3 :: Codiagonal Vector2 a
>     }
>   type (Vector3 \\ a) = Vector2 a
>   codiagonalImpl = codiag3
>   (|\|) = matrix3
>   downProject = downCodiagonal3
>   rightProject = rightCodiagonal3

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

>instance Indexable Vector3 a where
>  diagonalProjections = diagonal_projections3
>  indexableIndices = Vector3 0 1 2

>diagonal3 :: Matrix3 a -> Vector3 a
>diagonal3 (Matrix m) = Vector3 (xcoord3 $ xcoord3 m) (ycoord3 $ ycoord3 m) (zcoord3 $ zcoord3 m)

>diagonal_projections3 :: Vector3 (Index Vector3 a)
>diagonal_projections3 = Vector3 (xcoord3 <!-!> setx3)
>                                (ycoord3 <!-!> sety3)
>                                (zcoord3 <!-!> setz3)

>diagonal3x :: (Num a) => Matrix3 a -> Vector3 a
>diagonal3x (Matrix x) = fmap scalarProjection diagonal_projections3 <*> x
  
>diagonal3y :: (Num a) => Matrix3 a -> Vector3 a
>diagonal3y (Matrix x) = fmap scalarProjection (rotate_coordinates3 diagonal_projections3) <*> x
  
>diagonal3z :: (Num a) => Matrix3 a -> Vector3 a
>diagonal3z (Matrix x) = fmap scalarProjection (rotate_coordinates3 (rotate_coordinates3 diagonal_projections3)) <*> x

>-- | algorithm from <http://en.wikipedia.org/wiki/Determinant>

>determinant3 :: (Num a) => Matrix3 a -> a
>determinant3 (Matrix z@(Vector3 (Vector3 a b c) _ _)) =
>      a * det removex3 - b * det removey3 + c * det removez3
>  where det rc = determinantImpl (Matrix $ removex3 $ fmap rc z)

>determinant3_ :: (Num a) => Matrix3 a -> a
>determinant3_ (Matrix m) = combine $ pure (*) <*> xcoord3 m <*> amv
>   where amv = fmap (determinantImpl . Matrix . removex3 . (`fmap` m)) remove3
>         combine (Vector3 a b c) = a - b + c         


>instance (Num a, Ord a, ConjugateSymmetric a) => LinearTransform Vector1 Vector3 a where
>  x <*>> (Matrix (Vector1 m)) = Vector1 $ x %. m
>  (Matrix (Vector1 m)) <<*> (Vector1 x) = conj x %* m

>instance (Num a,ConjugateSymmetric a) => LinearTransform Vector3 Vector1 a where
>  (Vector1 x) <*>> (Matrix m) = x %* fmap (conj . vectorElement) m
>  (Matrix (Vector3 (Vector1 x) (Vector1 y) (Vector1 z))) <<*> (Vector3 a b c) = Vector1 (x*conj a+y*conj b+z*conj c) 

>instance (Num a, ConjugateSymmetric a) => LinearTransform Vector3 Vector2 a where
>  (<*>>) = leftMultiply2Gen
>  (<<*>) = rightMultiply3Gen

>instance (Num a, ConjugateSymmetric a) => LinearTransform Vector2 Vector3 a where
>  (<*>>) = leftMultiply3Gen
>  (<<*>) = rightMultiply2Gen

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

>instance (Num a) => VectorSpace ((Vector3 :*: Vector3) a) where
>  type Scalar ((Vector3 :*: Vector3) a) = a
>  vzero = Matrix $ Vector3 vzero vzero vzero
>  vnegate (Matrix v) = Matrix $ fmap negate v
>  v %* (Matrix x) = Matrix $ fmap (fmap (v *)) x
>  (Matrix x) %+ (Matrix y) = Matrix $ x %+ y

>instance {-# OVERLAPPING #-}
>     (Floating a, ConjugateSymmetric a, AdditiveIdentity a, MultiplicativeIdentity a)
> => NormedSpace ((Vector3 :*: Vector3) a) where
>  norm = innerproductspaceNorm

>instance (SupportsMatrixMultiplication Vector3 Vector3 Vector3 a)
> => InnerProductSpace ((Vector3 :*: Vector3) a) where
>  x %. y = traceImpl (transposeImpl x %*% y)

>instance (ConjugateSymmetric a, Num a) => InnerProductSpaceFunctor Vector3 a

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
>eigenvalue3 m = if p1 == 0 then diagonalImpl m else Vector3 eig1 eig2 eig3
>   where eig1 = q + 2*p*cos(phi)
>         eig3 = q + 2*p*cos(phi + 2*pi/3)
>         eig2 = 3*q - eig1 - eig3
>         pv = Vector3 (m <!> (xcoord3,ycoord3))
>                      (m <!> (xcoord3,zcoord3))
>                      (m <!> (ycoord3,zcoord3))
>         p1 = normSquared pv
>         q = traceImpl m / 3
>         mdq = diagonalImpl m - return q
>         p2 = normSquared mdq + 2 * p1
>         p = sqrt (p2 / 6)
>         b = (1 / p) %* (m %- q %* identity)
>         r = determinantImpl b / 2
>         phi | r <= -1   = pi / 3
>             | r >= 1    = 0
>             | otherwise = acos r / 3


>instance (Floating a, Ord a,ConjugateSymmetric a)
> => EigenDecomposable Vector3 a where
>   eigenvalues = eigenvalue3

>instance (Fractional a) => Invertible Vector3 a where
>   cofactorImpl = cofactor3
>   adjucateImpl = adjucate3
>   inverseImpl = inverse3
>
>-- | <https://en.wikipedia.org/wiki/Levi-Civita_symbol>
>levi_civita3 :: ((Vector3 :*: Vector3) :*: Vector3) Int
>levi_civita3 = matrix m (matrix (,) indexableIndices indexableIndices) indexableIndices
>  where m (x,y) z | x == 0 && y == 1 && z == 2 = 1
>                  | x == 1 && y == 2 && z == 0 = 1
>                  | x == 2 && y == 0 && z == 1 = 1
>                  | x == 2 && y == 1 && z == 0 = negate 1
>                  | x == 0 && y == 2 && z == 1 = negate 1
>                  | x == 1 && y == 0 && z == 2 = negate 1
>                  | x == y || y == z || z == x = 0

>instance (Limiting Stream a) => Limiting Stream (Vector3 a) where
>  data Closure Stream (Vector3 a) = Vector3Closure (Vector3 (Closure Stream a))
>  limit str = Vector3Closure $ Vector3
>                      (limit $ fmap xcoord3 str)
>                      (limit $ fmap ycoord3 str)
>                      (limit $ fmap zcoord3 str)
>  approximations (Vector3Closure (Vector3 a b c)) = do
>     (a',b',c') <- fzip3 (approximations a)
>                         (approximations b)
>                         (approximations c)
>     return $! Vector3 a' b' c'

>instance (Show a, Limiting Stream a) => Show (Closure Stream (Vector3 a)) where
>   showsPrec _ v = shows $ shead $ approximations v

>instance (PpShow a, Limiting Stream a) => PpShow (Closure Stream (Vector3 a)) where
>   pp v = pp $ shead $ approximations v

>cycle3 :: (StreamBuilder str) => Vector3 a -> str a
>cycle3 z@(Vector3 a b c) = a `pre` b `pre` c `pre` cycle3 z
