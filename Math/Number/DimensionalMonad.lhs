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
>   return = pure
>   (QuantityM f) >>= g = QuantityM $ \d -> g (f d) `runQuantifyM` d

>instance MonadFail QuantityM where
>   fail msg = QuantityM $ \d -> error $ "dimension " ++ show d ++ ":" ++ msg

>resultM :: QuantityM a -> QuantityM (Quantity a)
>resultM (QuantityM f) = QuantityM $ \d -> f d `As` d

>returnM :: Quantity a -> QuantityM a
>returnM (a `As` d) = QuantityM $ \d' ->
> if d == d' then a
>  else error $ "dimension mismatch: " ++ show d ++ " != " ++ show d'

>quantityM :: (MonadFail m) => Quantity a -> QuantityM (m a)
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


>-- | Monad transformed that add dimensional analysis
>-- The second argument to runDimensionalMT is the dimension of scalars.
>data DimensionalMT m r = DimensionalMT { runDimensionalMT :: Dimension -> m (Quantity r) }

>instance (Num r, Applicative m, VectorSpace r) => VectorSpace (DimensionalMT m r) where
>   type Scalar (DimensionalMT m r) = r
>   vzero = DimensionalMT $ \d -> pure (vzero `As` d)
>   vnegate (DimensionalMT f) = DimensionalMT $ \d -> fmap vnegate (f d)
>   (DimensionalMT f) %+ (DimensionalMT g) = DimensionalMT $ \d -> liftA2 (%+) (f d) (g d)
>   k %* (DimensionalMT f) = DimensionalMT $ \d -> fmap (k %*) (f d)

>scalarDimensionMT :: (Applicative m) => DimensionalMT m Dimension
>scalarDimensionMT = DimensionalMT $ \d -> pure (d `As` dimensionless)

>-- | quantityMT validates the dimension
>quantityMT :: (MonadFail m) => Quantity r -> DimensionalMT m r
>quantityMT (x `As` rd) = DimensionalMT $ \d ->
>   if d == rd then return (x `As` d) else fail $ "Invalid dimensions: [" ++ show d ++ "] != [" ++ show rd ++ "]"

>terminalMT :: (Monad m) => DimensionalMT m ()
>terminalMT = DimensionalMT $ \d -> return (() `As` d)

>instance (Functor m) => Functor (DimensionalMT m) where
>  fmap f (DimensionalMT x) = DimensionalMT $ \d -> fmap (fmap f) (x d)

>instance (Applicative m) => Applicative (DimensionalMT m) where
>   pure x = DimensionalMT $ \d -> pure (x `As` d)
>   (DimensionalMT f) <*> (DimensionalMT x) = DimensionalMT $ \d -> liftA2 (<*>) (f d) (x d)

>instance (Monad m) => Monad (DimensionalMT m) where
>  return = pure
>  (DimensionalMT g) >>= f = DimensionalMT $ \d -> do
>     (As v d') <- g d
>     f v `runDimensionalMT` d'

>instance (MonadFail m) => MonadFail (DimensionalMT m) where
>  fail msg = DimensionalMT $ \d -> fail $ msg ++ "\n  in context with dimension [" ++ show d ++ "]"

