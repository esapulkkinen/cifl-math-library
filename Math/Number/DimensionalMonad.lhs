>{-# LANGUAGE Safe, TypeFamilies #-}
>module Math.Number.DimensionalMonad where
>import safe Text.PrettyPrint (Doc)
>import safe qualified Text.PrettyPrint as Pretty
>import safe Math.Tools.PrettyP
>import safe Math.Matrix.Interface
>import safe Math.Number.DimensionalAnalysis

>-- | A monad that checks dimensions
>data QuantityM a = QuantityM { runQuantifyM :: Dimension -> a }

>instance (Num v) => VectorSpace (QuantityM v) where
>  type Scalar (QuantityM v) = v
>  vzero = QuantityM $ const 0
>  vnegate (QuantityM f) = QuantityM (negate . f)
>  (QuantityM f) %+ (QuantityM g) = QuantityM (\d -> f d + g d)
>  a %* (QuantityM f) = QuantityM $ \d -> a * f d

>instance Functor QuantityM where
>  fmap f (QuantityM g) = QuantityM (f . g)

>instance Applicative QuantityM where
>  pure x = QuantityM (const x)
>  (QuantityM f) <*> (QuantityM x) = QuantityM $ \d -> f d (x d)

>instance Monad QuantityM where
>   return x = returnM (x `As` dimensionless)
>   (QuantityM f) >>= g = QuantityM $ \d -> g (f d) `runQuantifyM` d
>   fail msg = QuantityM $ \d -> error $ "dimension " ++ show d ++ ":" ++ msg

>resultM :: QuantityM a -> QuantityM (Quantity a)
>resultM (QuantityM f) = QuantityM $ \d -> f d `As` d

>returnM :: Quantity a -> QuantityM a
>returnM (a `As` d) = QuantityM $ \d' ->
> if d == d' then a
>  else error $ "dimension mismatch: " ++ show d ++ " != " ++ show d'

>quantityM :: (Monad m) => Quantity a -> QuantityM (m a)
>quantityM (a `As` d) = QuantityM $ \d' -> if d == d' then return a
>   else fail $ "dimension mismatch: " ++ show d ++ " != " ++ show d'

>current_dimension :: QuantityM Dimension
>current_dimension = QuantityM id

>nested :: (Dimension -> Dimension) -> QuantityM a -> QuantityM a
>nested m (QuantityM f) = QuantityM $ fmap f m

>with :: QuantityM a -> Dimension -> QuantityM a
>with x d = nested (const d) x

>with_root :: Quantity (QuantityM a) -> QuantityM a
>with_root (a `As` d) = nested (/d) a

>with_power :: Quantity (QuantityM a) -> QuantityM a
>with_power (a `As` d) = nested (* d) a

>with_product :: Quantity (QuantityM a) -> QuantityM a
>with_product (a `As` d) = nested (%+ d) a

>with_divide :: Quantity (QuantityM a) -> QuantityM a
>with_divide (a `As` d) = nested (%- d) a

>dimension_name :: QuantityM String
>dimension_name = QuantityM show

>dimension_doc :: QuantityM Doc
>dimension_doc = QuantityM pp
