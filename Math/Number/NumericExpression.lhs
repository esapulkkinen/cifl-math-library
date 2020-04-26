>{-# LANGUAGE Safe, GADTs, UndecidableInstances, MultiParamTypeClasses, TypeOperators, TypeFamilies, LambdaCase, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, Arrows, Rank2Types, StandaloneDeriving, DeriveGeneric, DeriveDataTypeable #-}
>module Math.Number.NumericExpression where
>import safe Prelude hiding (id,(.))
>import safe Text.PrettyPrint ((<+>))
>import safe GHC.Generics hiding ((:*:), (:+:))
>import safe Data.Data
>import safe Data.Typeable
>import safe Control.Category
>import safe Control.Arrow hiding ((<+>))
>import safe Math.Tools.PrettyP
>import safe Data.Map (Map)
>import safe qualified Data.Map as Map
>import safe Control.Applicative
>import safe Math.Tools.Functor
>import safe Math.Tools.Show
>import safe Math.Tools.Visitor hiding (var)
>import safe Math.Tools.Functor
>import safe Math.Tools.Isomorphism
>import safe Math.Tools.Arrow
>import safe Math.Matrix.Interface
>import safe Math.Matrix.Matrix

>data Var a = Var { var_name :: !String, var_debruijn_index :: !Integer }
>           | Val !a
>   deriving (Typeable, Data, Generic, Eq)

>instance PpShowF Var where
>  ppf (Var x _) = pp x
>  ppf (Val x) = pp x

>instance VectorShow Var where
>   show_vector (Var x _) = x
>   show_vector (Val x) = show x

>instance FunctorArrow Var (->) where
>   amap f = proc z -> case z of
>     (Var n i) -> returnA -< (Var n i)
>     (Val x) -> f >>> arr Val -< x

>instance IsomorphicFunctor Var where
>  data IsoA Var a b = VarIso {
>    var_nameIso :: String :==: String,
>    var_indexIso :: Integer :==: Integer,
>    var_valIso :: a :==: b }
>  transformIso = varIso
>
>instance BiArrow (IsoA Var) where
>   f <-> g = VarIso id id (f <-> g)

>instance Category (IsoA Var) where
>   id = VarIso id id id
>   (VarIso n i x) . (VarIso n' i' x') = VarIso (n . n') (i . i') (x . x')
>   
>instance Groupoid (IsoA Var) where
>   invertA (VarIso n i x) = VarIso (invertA n) (invertA i) (invertA x)
>
>varIso :: IsoA Var a b -> Var a :==: Var b
>varIso v = iso v <-> iso (invertA v)
>  where iso v = \case { (Var n i) -> Var (isomorphism_epimorphism (var_nameIso v) n)
>                                         (isomorphism_epimorphism (var_indexIso v) i) ;
>                         (Val x) -> Val (isomorphism_epimorphism (var_valIso v) x) }

>instance (PpShow a) => PpShow (Var a) where
>   pp (Var x i) = pp x <> pp '@' <> pp i
>   pp (Val a) = pp a

>class Expression e ctx a where
>   eval :: ctx a -> e a -> a

>equal_in_context :: (Eq a, Expression f ctx a) => ctx a -> f a -> f a -> Bool
>equal_in_context ctx x y = eval ctx x == eval ctx y

>instance Visitor (Var a) where
>   data Fold (Var a) b = VarFold [a] (String -> a -> b) (a -> b)
>   visit (VarFold ctx f _) (Var n i) = f n (ctx !! fromIntegral i)
>   visit (VarFold _ _ g) (Val x) = g x

>evalVar :: [a] -> Var a -> a
>evalVar ctx (Var _ i) = ctx !! fromIntegral i
>evalVar _   (Val v) = v

>withCtx :: (Functor f) => [a] -> (f :*: Var) a -> f a
>withCtx ctx (Matrix expr) = fmap (evalVar ctx) expr

>eVar :: (Applicative f) => String -> Integer -> (f :*: Var) a
>eVar x i = Matrix $ pure (Var x i)

>expr :: (Functor f) => f a -> (f :*: Var) a
>expr e = Matrix $ fmap Val e

>instance (Show a) => Show (Var a) where
>  show (Var v i) = v ++ "@" ++ show i
>  show (Val x) = show x

>instance Functor Var where
>  fmap f (Var x i) = Var x i
>  fmap f (Val a) = Val (f a)

>data VectorCtx v a = VectorCtx { vectorSpaceCtx :: Map String (VectorSpaceExpr v a),
>                               numCtx :: [a]
>                             }
>   deriving (Eq, Data,Typeable,Generic)

instance (Num a, IdVisitor a) => Expression NumExpr VectorCtx a where
   eval ctx e = eval (numCtx ctx) e

>instance Expression Var (VectorCtx v) a where
>   eval ctx e = eval (numCtx ctx) e

>data VectorSpaceExpr v a = VZero
>                       | VNegate (VectorSpaceExpr v a)
>                       | VPlus (VectorSpaceExpr v a) (VectorSpaceExpr v a)
>                       | VScalar (NumExpr v a) (VectorSpaceExpr v a)
>                       | VVectorVar (Var (VectorSpaceExpr v a))
>                       | VPrim (v a)
>  deriving (Eq, Typeable, Data, Generic)

>instance (PpShowF v) => PpShowF (VectorSpaceExpr v) where
>   ppf VZero = pp "0"
>   ppf (VNegate x) = pp "-" <> ppf x
>   ppf (VPlus x y) = ppf x <+> pp "+" <+> ppf y
>   ppf (VScalar x v) = ppf x <+> pp "*" <+> ppf v
>   ppf (VVectorVar x) = ppf (fmap ppf x)
>   ppf (VPrim w) = ppf w

>instance (VectorShow v) => VectorShow (VectorSpaceExpr v) where
>   show_vector VZero = "0"
>   show_vector (VNegate x) = "-" ++ show_vector x
>   show_vector (VPlus x y) = show_vector x ++ "+" ++ show_vector y
>   show_vector (VScalar x v) = show_vector x ++ "*" ++ show_vector v
>   show_vector (VVectorVar x) = show_vector (fmap show_vector x)
>   show_vector (VPrim x) = show_vector x

>instance (FunctorArrow v (->)) => FunctorArrow (VectorSpaceExpr v) (->) where
>   amap f = proc z -> case z of
>       VZero -> returnA -< VZero
>       (VNegate v) -> do v' <- amap f -< v
>                         returnA -< VNegate v'
>       (VPlus x y) -> do x' <- amap f -< x
>                         y' <- amap f -< y
>                         returnA -< VPlus x' y'
>       (VScalar x v) -> do x' <- amap f -< x
>                           v' <- amap f -< v
>                           returnA -< VScalar x' v'
>       (VVectorVar v) -> do v' <- amap (amap f) -< v
>                            returnA -< VVectorVar v'
>       (VPrim x) -> do x' <- amap f -< x
>                       returnA -< VPrim x'

>instance (FunctorArrow v (->)) => IsomorphicFunctor (VectorSpaceExpr v) where
>  data IsoA (VectorSpaceExpr v) a b = VectorSpaceIso {
>     vnegate_iso :: IsoA (VectorSpaceExpr v) a b,
>     vplus_1_iso :: IsoA (VectorSpaceExpr v) a b,
>     vplus_2_iso :: IsoA (VectorSpaceExpr v) a b,
>     vscalar_1_iso :: IsoA (NumExpr v) a b,
>     vscalar_2_iso :: IsoA (VectorSpaceExpr v) a b,
>     vvector_1_iso :: IsoA Var (VectorSpaceExpr v a) (VectorSpaceExpr v b),
>     vprim_iso :: v a :==: v b }
>  transformIso = vectorSpaceIso

>vectorSpaceIso :: (FunctorArrow v (->)) => IsoA (VectorSpaceExpr v) a b -> (VectorSpaceExpr v a) :==: (VectorSpaceExpr v b)
>vectorSpaceIso v = iso v <-> iso (invertA v)
>   where iso v = \case
>           VZero -> VZero
>           (VNegate x) -> VNegate ((vnegate_iso v) `appIsoF` x)
>           (VPlus x y) -> VPlus ((vplus_1_iso v) `appIsoF` x)
>                                ((vplus_2_iso v) `appIsoF` y)
>           (VScalar x w) -> VScalar ((vscalar_1_iso v) `appIsoF` x)
>                                    ((vscalar_2_iso v) `appIsoF` w)
>           (VVectorVar w) -> VVectorVar ((vvector_1_iso v) `appIsoF` w)
>           (VPrim x) -> VPrim ((vprim_iso v) `isomorphism_epimorphism` x)

>instance (FunctorArrow v (->)) => BiArrow (IsoA (VectorSpaceExpr v)) where
>   f <-> g = VectorSpaceIso (f <-> g) (f <-> g) (f <-> g)
>                           (f <-> g) (f <-> g) (amap f <-> amap g) (amap f <-> amap g)

>instance (FunctorArrow v (->)) => Category (IsoA (VectorSpaceExpr v)) where
>   id = VectorSpaceIso id id id id id id id
>   (f :: IsoA (VectorSpaceExpr v) b' c') . (g :: IsoA (VectorSpaceExpr v) a' b') =
>     VectorSpaceIso (vnegate_iso f . vnegate_iso g)
>                          (vplus_1_iso f . vplus_1_iso g)
>                          (vplus_2_iso f . vplus_2_iso g)
>                          (vscalar_1_iso f . vscalar_1_iso g)
>                          (vscalar_2_iso f . vscalar_2_iso g)
>                          (vvector_1_iso f . vvector_1_iso g)
>                          (vprim_iso f . vprim_iso g)
>
>instance (FunctorArrow v (->)) => Groupoid (IsoA (VectorSpaceExpr v)) where
>   invertA v = VectorSpaceIso (invertA (vnegate_iso v))
>                              (invertA (vplus_1_iso v))
>                              (invertA (vplus_2_iso v))
>                              (invertA (vscalar_1_iso v))
>                              (invertA (vscalar_2_iso v))
>                              (invertA (vvector_1_iso v))
>                              (invertA (vprim_iso v))

>instance (PpShow a, PpShowF v) => PpShow (VectorSpaceExpr v a) where
>   pp VZero = pp '0'
>   pp (VNegate a) = pp '-' <> pp a
>   pp (VPlus x y) = pp x <+> pp '+' <+> pp y
>   pp (VScalar a x) = pp a <> pp "%*" <> pp x
>   pp (VVectorVar x) = pp x
>   pp (VPrim a) = ppf a

>instance Visitor (VectorSpaceExpr v a) where
>   data Fold (VectorSpaceExpr v a) b = VectorSpaceExprFold {
>       vzero_fold :: b,
>       vnegate_fold :: b -> b,
>       vplus_fold :: b -> b -> b,
>       vscalar_fold :: NumExpr v a -> b -> b,
>       vvector_fold :: Var b -> b,
>       vprim_fold :: v a -> b
>     }
>   visit z = \case
>     VZero -> vzero_fold z
>     (VNegate e) -> vnegate_fold z (visit z e)
>     (VPlus x y) -> vplus_fold z (visit z x) (visit z y)
>     (VScalar n e) -> vscalar_fold z n (visit z e)
>     (VVectorVar v) -> vvector_fold z (fmap (visit z) v)
>     (VPrim v) -> vprim_fold z v

>instance Expression Var [] a where
>   eval = evalVar

>evalVectorSpaceExpr :: (VectorSpace (v a), IdVisitor a,
>                       Expression Var (Map String) (VectorSpaceExpr v a),
>                        Scalar (v a) ~ a)
>                    => VectorCtx v a -> VectorSpaceExpr v a -> v a
>evalVectorSpaceExpr ctx = \case
> VZero -> vzero
> (VNegate e) -> vnegate (evalVectorSpaceExpr ctx e)
> (VPlus x y) -> evalVectorSpaceExpr ctx x %+ evalVectorSpaceExpr ctx y
> (VVectorVar x) -> evalVectorSpaceExpr ctx (eval (vectorSpaceCtx ctx) x)
> (VScalar s v) -> runNumExpr (numCtx ctx) s %* evalVectorSpaceExpr ctx v
> (VPrim a) -> a

instance (VectorSpace a) => Expression VectorSpaceExpr VectorCtx a where
   eval = evalVectorSpaceExpr

>deriving instance (Show a, Show (v a)) => Show (VectorSpaceExpr v a)

deriving instance (Show a) => Show ((NumExpr v :*: Var) a)

>-- | Basically same as methods in Num type class from prelude, but as data.
>data NumExpr v a = Plus !(NumExpr v a) !(NumExpr v a)
>            | Product !(NumExpr v a) !(NumExpr v a)
>            | Subtract !(NumExpr v a) !(NumExpr v a)
>            | Negate !(NumExpr v a)
>            | Abs !(NumExpr v a)
>            | Signum !(NumExpr v a)
>            | NumPrim !(Var a)
>            | FromInteger !Integer
>            | InnerProduct !(VectorSpaceExpr v a) !(VectorSpaceExpr v a)
>  deriving (Eq, Typeable, Data, Generic)

>instance (PpShowF v) => PpShowF (NumExpr v) where
>   ppf (Plus x y) = ppf x <+> pp "+" <+> ppf y
>   ppf (Product x y) = ppf x <> pp "*" <> ppf y
>   ppf (Subtract x y) = ppf x <+> pp "-" <+> ppf y
>   ppf (Negate x) = pp "-" <> ppf x
>   ppf (Abs x) = pp "abs(" <> ppf x <> pp ")"
>   ppf (Signum x) = pp "signum(" <> ppf x <> pp ")"
>   ppf (NumPrim x) = pp x
>   ppf (FromInteger x) = pp x
>   ppf (InnerProduct x y) = pp "innerproduct(" <> ppf x <> pp "," <> ppf y <> pp ")"

>instance (VectorShow v) => VectorShow (NumExpr v) where
>  show_vector (Plus x y) = show_vector x ++ " + " ++ show_vector y
>  show_vector (Product x y) = show_vector x ++ "*" ++ show_vector y
>  show_vector (Subtract x y) = show_vector x ++ " - " ++ show_vector y
>  show_vector (Negate x) = "-" ++ show_vector x
>  show_vector (Abs x) = "abs(" ++ show_vector x ++ ")"
>  show_vector (Signum x) = "signum(" ++ show_vector x ++ ")"
>  show_vector (NumPrim x) = show x
>  show_vector (FromInteger x) = show x
>  show_vector (InnerProduct x y) = "innerproduct(" ++ show_vector x ++ "," ++ show_vector y ++ ")"

>instance (FunctorArrow v (->)) => FunctorArrow (NumExpr v) (->) where
>   amap f = proc z -> case z of
>     (Plus x y) -> run Plus -< (x,y)
>     (Product x y) -> run Product -< (x,y)
>     (Subtract x y) -> run Subtract -< (x,y)
>     (Negate x) -> amap f >>> arr Negate -< x
>     (Abs x) -> amap f >>> arr Abs -< x
>     (NumPrim v) -> amap f >>> arr NumPrim -< v
>     (FromInteger i) -> returnA -< FromInteger i
>     (InnerProduct x y) -> arr (uncurry InnerProduct) <<< (amap f *** amap f) -< (x,y)
>    where run p = arr (uncurry p) <<< (amap f *** amap f)
                      
>instance (FunctorArrow v (->)) => IsomorphicFunctor (NumExpr v) where
>  data IsoA (NumExpr v) a b = NumIso {
>   plusIso_1 :: IsoA (NumExpr v) a b,
>   plusIso_2 :: IsoA (NumExpr v) a b,
>   prodIso_1 :: IsoA (NumExpr v) a b,
>   prodIso_2 :: IsoA (NumExpr v) a b,
>   subtractIso_1 :: IsoA (NumExpr v) a b,
>   subtractIso_2 :: IsoA (NumExpr v) a b,
>   negateIso :: IsoA (NumExpr v) a b,
>   numAbsIso :: IsoA (NumExpr v) a b,
>   signumIso :: IsoA (NumExpr v) a b,
>   numPrimIso :: IsoA Var a b,
>   fromIntegerIso :: Integer :==: Integer,
>   innerProductIso_1 :: IsoA (VectorSpaceExpr v) a b,
>   innerProductIso_2 :: IsoA (VectorSpaceExpr v) a b }
>  transformIso = numIso


>numIso :: (FunctorArrow v (->)) => IsoA (NumExpr v) a b -> NumExpr v a :==: NumExpr v b
>numIso v = iso v <-> iso (invertA v)
>  where iso v = \case
>          (Plus x y) -> Plus ((plusIso_1 v) `appIsoF` x)
>                             ((plusIso_2 v) `appIsoF` y)
>          (Product x y) -> Product ((prodIso_1 v) `appIsoF` x)
>                                   ((prodIso_2 v) `appIsoF` y)
>          (Negate x) -> Negate ((negateIso v) `appIsoF` x)
>          (Abs x) -> Abs ((numAbsIso v) `appIsoF` x)
>          (Signum x) -> Signum ((signumIso v) `appIsoF` x)
>          (NumPrim x) -> NumPrim ((numPrimIso v) `appIsoF` x)
>          (FromInteger i) -> FromInteger ((fromIntegerIso v) `isomorphism_epimorphism` i)
>          (InnerProduct x y) -> InnerProduct ((innerProductIso_1 v) `appIsoF` x)
>                                             ((innerProductIso_2 v) `appIsoF` y)

>instance (FunctorArrow v (->)) => BiArrow (IsoA (NumExpr v)) where
>   f <-> g = NumIso niso niso niso niso niso niso niso niso niso (f <-> g)
>                   id (f <-> g) (f <-> g)
>     where niso = f <-> g


>instance (FunctorArrow v (->)) => Groupoid (IsoA (NumExpr v)) where
>   invertA f = NumIso (invertA (plusIso_1 f))
>                      (invertA (plusIso_2 f))
>                      (invertA (prodIso_1 f))
>                      (invertA (prodIso_2 f))
>                      (invertA (subtractIso_1 f))
>                      (invertA (subtractIso_2 f))
>                      (invertA (negateIso f))
>                      (invertA (numAbsIso f))
>                      (invertA (signumIso f))
>                      (invertA (numPrimIso f))
>                      (invertA (fromIntegerIso f))
>                      (invertA (innerProductIso_1 f))
>                      (invertA (innerProductIso_2 f))

>instance (FunctorArrow v (->)) => Category (IsoA (NumExpr v)) where
>   id = NumIso id id id id id id id id id id id id id
>   f . g = NumIso (plusIso_1 f . plusIso_1 g)
>                  (plusIso_2 f . plusIso_2 g)
>                  (prodIso_1 f . prodIso_1 g)
>                  (prodIso_2 f . prodIso_2 g)
>                  (subtractIso_1 f . subtractIso_1 g)
>                  (subtractIso_2 f . subtractIso_2 g)
>                  (negateIso f . negateIso g)
>                  (numAbsIso f . numAbsIso g)
>                  (signumIso f . signumIso g)
>                  (numPrimIso f . numPrimIso g)
>                  (fromIntegerIso f . fromIntegerIso g)
>                  (innerProductIso_1 f . innerProductIso_1 g)
>                  (innerProductIso_2 f . innerProductIso_2 g)



>plus_assoc_iso :: NumExpr v a :==: NumExpr v a
>plus_assoc_iso = \case { (Plus (Plus x y) z) -> Plus x (Plus y z) ;  z -> z }
>              <-> \case { (Plus x (Plus y z)) -> Plus (Plus x y) z ; z -> z }

>product_assoc_iso :: NumExpr v a :==: NumExpr v a
>product_assoc_iso = \case { (Product (Product x y) z) -> Product x (Product y z) ; z -> z }
>       <-> \case { (Product x (Product y z)) -> Product (Product x y) z ; z -> z }

>instance (PpShow a, PpShowF v) => PpShow (NumExpr v a) where
>   pp (Plus a b) = pp a <> pp '+' <> pp b
>   pp (Product a b) = pp a <> pp '*' <> pp b
>   pp (Subtract a b) = pp a <> pp '-' <> pp b
>   pp (Negate a) = pp '-' <> pp a
>   pp (Abs a) = pp_unary "abs" a
>   pp (Signum a) = pp_unary "signum" a
>   pp (NumPrim a) = pp a
>   pp (FromInteger a) = pp a
>   pp (InnerProduct a b) = pp '<' <> pp a <> pp '|' <> pp b <> pp '>'

>instance InnerProductSpace (VectorSpaceExpr v a) where
>     x %. y = InnerProduct x y

>instance (Eq a, Show a) => ConjugateSymmetric (NumExpr v a) where
>   conj x = x

>instance (Num a, IdVisitor a) => Expression (NumExpr v) [] a where
>   eval = runNumExpr

>instance (Fractional a,IdVisitor a) => Expression (FracExpr v) [] a where
>   eval = runFracExpr
>
>instance (Enum a) => Expression EnumExpr [] a where
>   eval = runEnumExpr

>-- | basically same as Fractional class in Prelude.
>data FracExpr v a = PrimFracExpr !(NumExpr v a)
>                | Divide !(FracExpr v a) !(FracExpr v a)
>                | Recip  !(FracExpr v a)
>                | FromRational !Rational
>  deriving (Eq, Typeable, Data, Generic)

>instance (PpShowF v) => PpShowF (FracExpr v) where
>   ppf (PrimFracExpr v) = ppf v
>   ppf (Divide x y) = ppf x <+> pp "/" <+> ppf y
>   ppf (Recip x) = pp "1/" <> ppf x
>   ppf (FromRational x) = pp x

>instance (PpShow a, PpShowF v) => PpShow (FracExpr v a) where
>   pp (PrimFracExpr a) = ppf a
>   pp (Divide a b) = pp a <> pp '/' <> pp b
>   pp (Recip a) = pp "1/" <> pp a
>   pp (FromRational a) = pp a

>-- | basically same as Floating class in Prelude, but as data.
>data FloatExpr v a = PrimPi
>                 | Exp !(FloatExpr v a)
>                 | Log !(FloatExpr v a)
>                 | Sqrt !(FloatExpr v a)
>                 | Power !(FloatExpr v a) !(FloatExpr v a)
>                 | Sin !(FloatExpr v a)
>                 | Cos !(FloatExpr v a)
>                 | Tan !(FloatExpr v a)
>                 | Asin !(FloatExpr v a)
>                 | Acos !(FloatExpr v a)
>                 | Atan !(FloatExpr v a)
>                 | Sinh !(FloatExpr v a)
>                 | Cosh !(FloatExpr v a)
>                 | Tanh !(FloatExpr v a)
>                 | Asinh !(FloatExpr v a)
>                 | Acosh !(FloatExpr v a)
>                 | Atanh !(FloatExpr v a)
>                 | PrimFloatExpr !(FracExpr v (FloatExpr v a))
>  deriving (Generic)

>deriving instance (Typeable (v (FloatExpr v a))) => Typeable (FloatExpr v a)
>deriving instance (Eq (v (FloatExpr v a))) => Eq (FloatExpr v a)
>deriving instance (Typeable v, Typeable a, Data (v (FloatExpr v a))) => Data (FloatExpr v a)

>pp_unary op a = pp op <> pp '(' <> pp a <> pp ')'

>instance (PpShowF v) => PpShow (FloatExpr v a) where
>   pp PrimPi = pp "pi"
>   pp (Exp a) = pp_unary "exp" a
>   pp (Log a) = pp_unary "log" a
>   pp (Sqrt a) = pp_unary "sqrt" a
>   pp (Sin a) = pp_unary "sin" a
>   pp (Cos a) = pp_unary "cos" a
>   pp (Tan a) = pp_unary "tan" a
>   pp (Asin a) = pp_unary "asin" a
>   pp (Acos a) = pp_unary "acos" a
>   pp (Atan a) = pp_unary "atan" a
>   pp (Asinh a) = pp_unary "asinh" a
>   pp (Acosh a) = pp_unary "acosh" a
>   pp (Atanh a) = pp_unary "atanh" a
>   pp (Sinh a) = pp_unary "sinh" a
>   pp (Cosh a) = pp_unary "cosh" a
>   pp (Tanh a) = pp_unary "tanh" a
>   pp (PrimFloatExpr a) = ppf a

>class VectorShow v where
>   show_vector :: (Show a) => v a -> String

>instance (VectorShow v) => Show (FloatExpr v a) where
>   show PrimPi = "pi"
>   show (Exp a) = "exp(" ++ show a ++ ")"
>   show (Log a) = "log(" ++ show a ++ ")"
>   show (Sqrt a) = "sqrt(" ++ show a ++ ")"
>   show (Power a b) = "(" ++ show a ++ ")^(" ++ show b ++ ")"
>   show (Sin a) = "sin(" ++ show a ++ ")"
>   show (Cos a) = "cos(" ++ show a ++ ")"
>   show (Tan a) = "tan(" ++ show a ++ ")"
>   show (Asin a) = "asin(" ++ show a ++ ")"
>   show (Acos a) = "acos(" ++ show a ++ ")"
>   show (Atan a) = "atan(" ++ show a ++ ")"
>   show (Cosh a) = "cosh(" ++ show a ++ ")"
>   show (Sinh a) = "sinh(" ++ show a ++ ")"
>   show (Tanh a) = "tanh(" ++ show a ++ ")"
>   show (Asinh a) = "asinh(" ++ show a ++ ")"
>   show (Acosh a) = "acosh(" ++ show a ++ ")"
>   show (Atanh a) = "atanh(" ++ show a ++ ")"
>   show (PrimFloatExpr a) = show_vector a

>liftNumFrac1 f x = PrimFloatExpr $ PrimFracExpr $ f (NumPrim (Val x))

>liftNumFrac2 op x y = PrimFloatExpr $ PrimFracExpr $ op (NumPrim (Val x))
>                                                        (NumPrim (Val y))

>instance Num (FloatExpr v a) where
>   (+) = liftNumFrac2 Plus 
>   (-) = liftNumFrac2 Subtract
>   (*) = liftNumFrac2 Product
>   negate = liftNumFrac1 Negate
>   abs = liftNumFrac1 Abs
>   signum = liftNumFrac1 Signum
>   fromInteger i = PrimFloatExpr $ PrimFracExpr $ FromInteger i

>instance Fractional (FloatExpr v a) where
>   x / y = PrimFloatExpr $ Divide (PrimFracExpr (NumPrim (Val x)))
>                                  (PrimFracExpr (NumPrim (Val y)))
>   recip x = PrimFloatExpr $ Recip (PrimFracExpr (NumPrim (Val x)))
>   fromRational x = PrimFloatExpr (FromRational x)

>instance Floating (FloatExpr v a) where
>   pi = PrimPi
>   exp = Exp
>   log = Log
>   sqrt = Sqrt
>   (**) = Power
>   sin = Sin
>   cos = Cos
>   tan = Tan
>   asin = Asin
>   acos = Acos
>   atan = Atan
>   sinh = Sinh
>   cosh = Cosh
>   tanh = Tanh
>   asinh = Asinh
>   acosh = Acosh
>   atanh = Atanh

>data EnumExpr a = SuccExpr !(EnumExpr a)
>                | PredExpr !(EnumExpr a)
>                | ToEnumExpr !(Var a)
>  deriving (Eq, Typeable, Data, Generic)

>instance (Enum a) => Enum (EnumExpr a) where
>  succ = SuccExpr
>  pred = PredExpr
>  toEnum i = ToEnumExpr $ Val (toEnum i)
>  fromEnum (SuccExpr x) = succ $ fromEnum x
>  fromEnum (PredExpr x) = pred $ fromEnum x
>  fromEnum (ToEnumExpr (Val i)) = fromEnum i
>  fromEnum (ToEnumExpr (Var _ _)) = error "fromEnum: not supported for variables"

>instance Functor (FracExpr v) where
>   fmap f (PrimFracExpr x) = PrimFracExpr (fmap f x)
>   fmap f (Divide x y) = Divide (fmap f x) (fmap f y)
>   fmap f (Recip x) = Recip (fmap f x)
>   fmap f (FromRational r) = FromRational r

>instance (VectorShow v) => VectorShow (FracExpr v) where
>   show_vector (PrimFracExpr ne) = show_vector ne
>   show_vector (Divide x y) = show_vector x ++ "/" ++ show_vector y
>   show_vector (Recip x) = "1/(" ++ show_vector x ++ ")"
>   show_vector (FromRational x) = show x

>instance (Show a, Show (v a)) => Show (FracExpr v a) where
>   showsPrec p (PrimFracExpr ne) = showsPrec p ne
>   showsPrec p (Divide x y) = showsBinaryOp 4 "/" p x y
>   showsPrec p (Recip x) = showString "recip(" . showsPrec 0 x . showChar ')'

>instance (Show a, Show (v a)) => Show (NumExpr v a) where
>   showsPrec p (Plus x y) = showsBinaryOp 3 "+" p x y
>   showsPrec p (Product x y) = showsBinaryOp 4 "*" p x y
>   showsPrec p (Subtract x y) = showsBinaryOp 3 "-" p x y
>   showsPrec p (Negate x)     = showChar '-' . showsPrec 11 x
>   showsPrec p (Abs x)        = showString "abs(" . showsPrec 0 x . showChar ')'
>   showsPrec p (Signum x)     = showString "signum(" . showsPrec 0 x . showChar ')'
>   showsPrec p (NumPrim x)       = showsPrec p x
>   showsPrec p (FromInteger i) = showsPrec p i
>   showsPrec p (InnerProduct x y) = showString "innerproduct(" . showsPrec 0 x . showChar ',' . showsPrec 0 y . showChar ')'

>instance Functor (NumExpr v) where
>   fmap f (Plus x y) = Plus (fmap f x) (fmap f y)
>   fmap f (Product x y) = Product (fmap f x) (fmap f y)
>   fmap f (Subtract x y) = Subtract (fmap f x) (fmap f y)
>   fmap f (Negate x) = Negate (fmap f x)
>   fmap f (Abs x) = Abs (fmap f x)
>   fmap f (Signum x) = Signum (fmap f x)
>   fmap f (NumPrim x) = NumPrim (fmap f x)
>   fmap f (FromInteger i) = FromInteger i

>instance Visitor (FracExpr v a) where
>  data Fold (FracExpr v a) b = FracExprFold {
>             fracexprfold_prim :: Fold (NumExpr v a) b,
>             fracexprfold_divide :: b -> b -> b,
>             fracexprfold_recip  :: b -> b,
>             fracexprfold_fromrational :: Rational -> b }
>  visit z (PrimFracExpr p) = visit (fracexprfold_prim z) p
>  visit z (Divide x y) = fracexprfold_divide z (visit z x) (visit z y)
>  visit z (Recip x)    = fracexprfold_recip z (visit z x)
>  visit z (FromRational r) = fracexprfold_fromrational z r

>instance Visitor (FloatExpr v a) where
>   data Fold (FloatExpr v a) b = FloatExprFold {
>     floatfold_pi :: b,
>     floatfold_exp :: b -> b,
>     floatfold_log :: b -> b,
>     floatfold_sqrt :: b -> b,
>     floatfold_power :: b -> b -> b,
>     floatfold_sin :: b -> b,
>     floatfold_cos :: b -> b,
>     floatfold_tan :: b -> b,
>     floatfold_asin :: b -> b,
>     floatfold_acos :: b -> b,
>     floatfold_atan :: b -> b,
>     floatfold_sinh :: b -> b,
>     floatfold_cosh :: b -> b,
>     floatfold_tanh :: b -> b,
>     floatfold_asinh :: b -> b,
>     floatfold_acosh :: b -> b,
>     floatfold_atanh :: b -> b,
>     floatfold_prim :: Fold (FracExpr v (FloatExpr v a)) b }
>   visit z = \case
>     PrimPi -> floatfold_pi z
>     (Exp x) -> floatfold_exp z (visit z x)
>     (Log x) -> floatfold_log z (visit z x)
>     (Sqrt x) -> floatfold_sqrt z (visit z x)
>     (Power x y) -> floatfold_power z (visit z x) (visit z x)
>     (Sin x) -> floatfold_sin z (visit z x)
>     (Cos x) -> floatfold_cos z (visit z x)
>     (Tan x) -> floatfold_tan z (visit z x)
>     (Asin x) -> floatfold_asin z (visit z x)
>     (Acos x) -> floatfold_acos z (visit z x)
>     (Atan x) -> floatfold_atan z (visit z x)
>     (Sinh x) -> floatfold_sinh z (visit z x)
>     (Cosh x) -> floatfold_cosh z (visit z x)
>     (Tanh x) -> floatfold_tanh z (visit z x)
>     (Asinh x) -> floatfold_asinh z (visit z x)
>     (Acosh x) -> floatfold_acosh z (visit z x)
>     (Atanh x) -> floatfold_atanh z (visit z x)
>     (PrimFloatExpr fr) -> visit (floatfold_prim z) fr

>instance Visitor (NumExpr v a) where
>  data Fold (NumExpr v a) b = NumExprFold { exprfold_plus :: b -> b -> b,
>                                    exprfold_product :: b -> b -> b,
>                                    exprfold_subtract :: b -> b -> b,
>                                    exprfold_negate   :: b -> b,
>                                    exprfold_abs      :: b -> b,
>                                    exprfold_signum   :: b -> b,
>                                    exprfold_prim     :: Fold (Var a) b,
>                                    exprfold_frominteger :: Integer -> b }
>  visit z = \case
>    (Plus x y) -> exprfold_plus z (visit z x) (visit z y)
>    (Product x y) -> exprfold_product z (visit z x) (visit z y)
>    (Subtract x y) -> exprfold_subtract z (visit z x) (visit z y)
>    (Negate x)     -> exprfold_negate z (visit z x)
>    (Abs x)        -> exprfold_abs z (visit z x)
>    (Signum x)     -> exprfold_signum z (visit z x)
>    (NumPrim a)    -> visit (exprfold_prim z) a
>    (FromInteger i) -> exprfold_frominteger z i



>instance Visitor (EnumExpr a) where
>   data Fold (EnumExpr a) b = EnumExprFold {
>      enumfold_succ :: b -> b,
>      enumfold_pred :: b -> b,
>      enumfold_toenum :: Fold (Var a) b }
>   visit z = \case
>     (SuccExpr x) -> enumfold_succ z (visit z x)
>     (PredExpr x) -> enumfold_pred z (visit z x)
>     (ToEnumExpr i) -> visit (enumfold_toenum z) i

>runEnumExpr :: (Enum a) => [a] -> EnumExpr a -> a
>runEnumExpr ctx = visit (enumExprFold $ varFold ctx)

>runNumExpr :: (Num a, IdVisitor a) => [a] -> NumExpr v a -> a
>runNumExpr ctx = visit $ numExprFold (varFold ctx)

>runFracExpr :: (Fractional a, IdVisitor a) => [a] -> FracExpr v a -> a
>runFracExpr ctx = visit $ fracExprFold $ numExprFold $ varFold ctx

>varFold :: [a] -> Fold (Var a) a
>varFold ctx = VarFold ctx (const id) id

>vecSpaceFold :: (Visitor (v a), VectorSpace b) =>
>                Fold (Var b) b
>             -> Fold (NumExpr v a) (Scalar b)
>             -> Fold (v a) b
>             -> Fold (VectorSpaceExpr v a) b
>vecSpaceFold varFold numFold primFold = VectorSpaceExprFold vzero vnegate (%+)
>                          (\e x -> visit numFold e %* x)
>                          (visit varFold) (visit primFold)

>numExprFold :: (Num a) => Fold (Var a) a -> Fold (NumExpr v a) a
>numExprFold varFold = NumExprFold (+) (*) (-) negate abs signum
>                                  varFold 
>                                  fromInteger

>fracExprFold :: (Fractional b) =>
>                Fold (NumExpr v a) b -> Fold (FracExpr v a) b
>fracExprFold numFold = FracExprFold numFold (/) recip fromRational

>enumExprFold :: (Enum b) => Fold (Var a) b -> Fold (EnumExpr a) b
>enumExprFold vFold = EnumExprFold succ pred vFold

>instance Num (NumExpr v a) where
>   (+) = Plus
>   (*) = Product
>   (-) = Subtract
>   negate = Negate
>   abs = Abs
>   signum = Signum
>   fromInteger = FromInteger

>instance Num ((NumExpr v :*: Var) a) where
>  x + y = Matrix $ Plus (cells x) (cells y)
>  x - y = Matrix $ Subtract (cells x) (cells y)
>  x * y = Matrix $ Product (cells x) (cells y)
>  negate x = Matrix $ Negate (cells x)
>  abs x = Matrix $ Abs (cells x)
>  signum x = Matrix $ Signum (cells x)
>  fromInteger i = Matrix $ FromInteger i


>instance VectorSpace (VectorSpaceExpr v a) where
>   type Scalar (VectorSpaceExpr v a) = NumExpr v a
>   vzero = VZero
>   vnegate = VNegate
>   (%+) = VPlus
>   s %* v = VScalar s v

