>{-# LANGUAGE GADTs, MultiParamTypeClasses, StandaloneDeriving, FlexibleInstances, UndecidableInstances, FlexibleContexts, TypeOperators, TypeFamilies, ExistentialQuantification #-}
>module Math.Tools.NumericExpression where
>import Math.Tools.PrettyP
>import Data.Map (Map)
>import qualified Data.Map as Map
>import Control.Applicative
>import Math.Tools.Functor
>import Math.Tools.Show
>import Math.Tools.Visitor hiding (var)
>import Math.Tools.Functor
>import Math.Matrix.Interface
>import Math.Matrix.Matrix

>data Var a = Var { var_name :: String, var_debruijn_index :: Integer }
>           | Val a

>instance (PpShow a) => PpShow (Var a) where
>   pp (Var x i) = pp x <> pp '@' <> pp i
>   pp (Val a) = pp a

>class Expression e ctx a where
>   eval :: ctx a -> e a -> a

>equal_in_context :: (Eq a, Expression f ctx a) => ctx a -> f a -> f a -> Bool
>equal_in_context ctx x y = eval ctx x == eval ctx y

>instance Visitor (Var a) where
>   data Fold (Var a) b = VarFold [a] (a -> b)
>   visit (VarFold ctx f) (Var _ i) = f (ctx !! fromIntegral i)
>   visit (VarFold _ f) (Val x) = f x

>evalVar :: [a] -> Var a -> a
>evalVar ctx (Var _ i) = ctx !! fromIntegral i
>evalVar _   (Val v) = v

>withCtx :: (Functor f) => [a] -> (f :*: Var) a -> f a
>withCtx ctx (Matrix expr) = fmap (evalVar ctx) expr

>var :: (Applicative f) => String -> Integer -> (f :*: Var) a
>var x i = Matrix $ pure (Var x i)

>expr :: (Functor f) => f a -> (f :*: Var) a
>expr e = Matrix $ fmap Val e

>instance (Show a) => Show (Var a) where
>  show (Var v i) = v ++ "@" ++ show i
>  show (Val x) = show x

>instance Functor Var where
>  fmap f (Var x i) = Var x i
>  fmap f (Val a) = Val (f a)

>data VectorCtx a = VectorCtx { vectorSpaceCtx :: Map String (VectorSpaceExpr a),
>                               scalarCtx :: Map String (NumExpr a),
>                               numericScalarCtx :: VectorCtx (NumExpr a),
>                               numCtx :: [a]
>                             }


>instance (Num a, IdVisitor a) => Expression NumExpr VectorCtx a where
>   eval ctx e = eval (numCtx ctx) e

>instance Expression Var VectorCtx a where
>   eval ctx e = eval (numCtx ctx) e

>data VectorSpaceExpr a = VZero
>                       | VNegate (VectorSpaceExpr a)
>                       | VPlus (VectorSpaceExpr a) (VectorSpaceExpr a)
>                       | VScalar (NumExpr a) (VectorSpaceExpr a)
>                       | VVectorVar (Var (VectorSpaceExpr a))
>                       | VPrim a

>instance (PpShow a) => PpShow (VectorSpaceExpr a) where
>   pp VZero = pp '0'
>   pp (VNegate a) = pp '-' <> pp a
>   pp (VPlus x y) = pp x <+> pp '+' <+> pp y
>   pp (VScalar a x) = pp a <> pp "%*" <> pp x
>   pp (VVectorVar x) = pp x
>   pp (VPrim a) = pp a

>instance Visitor (VectorSpaceExpr a) where
>   data Fold (VectorSpaceExpr a) b = VectorSpaceExprFold {
>       vzero_fold :: b,
>       vnegate_fold :: b -> b,
>       vplus_fold :: b -> b -> b,
>       vscalar_fold :: NumExpr a -> b -> b,
>       vvector_fold :: Var b -> b,
>       vprim_fold :: a -> b
>     }
>   visit z VZero = vzero_fold z
>   visit z (VNegate e) = vnegate_fold z (visit z e)
>   visit z (VPlus x y) = vplus_fold z (visit z x) (visit z y)
>   visit z (VScalar n e) = vscalar_fold z n (visit z e)
>   visit z (VVectorVar v) = vvector_fold z (fmap (visit z) v)
>   visit z (VPrim v) = vprim_fold z v

>instance Expression Var [] a where
>   eval = evalVar

>evalVectorSpaceExpr ctx VZero = vzero
>evalVectorSpaceExpr ctx (VNegate e) = vnegate (evalVectorSpaceExpr ctx e)
>evalVectorSpaceExpr ctx (VPlus x y) = evalVectorSpaceExpr ctx x %+ evalVectorSpaceExpr ctx y
>evalVectorSpaceExpr ctx (VVectorVar x) = evalVectorSpaceExpr ctx (eval (vectorSpaceCtx ctx) x)
>evalVectorSpaceExpr ctx (VScalar s v) = eval ctx s %* eval ctx v
>evalVectorSpaceExpr ctx (VPrim a) = a

instance Expression VectorSpaceExpr VectorCtx a where
   eval = evalVectorSpaceExpr

>deriving instance (Show a, Show (Scalar a)) => Show (VectorSpaceExpr a)
>deriving instance (Show a) => Show ((NumExpr :*: Var) a)

>data NumExpr a = Plus (NumExpr a) (NumExpr a)
>            | Product (NumExpr a) (NumExpr a)
>            | Subtract (NumExpr a) (NumExpr a)
>            | Negate (NumExpr a)
>            | Abs (NumExpr a)
>            | Signum (NumExpr a)
>            | NumPrim (Var a)
>            | FromInteger Integer
>            | InnerProduct (VectorSpaceExpr a) (VectorSpaceExpr a)

>instance (PpShow a) => PpShow (NumExpr a) where
>   pp (Plus a b) = pp a <> pp '+' <> pp b
>   pp (Product a b) = pp a <> pp '*' <> pp b
>   pp (Subtract a b) = pp a <> pp '-' <> pp b
>   pp (Negate a) = pp '-' <> pp a
>   pp (Abs a) = pp_unary "abs" a
>   pp (Signum a) = pp_unary "signum" a
>   pp (NumPrim a) = pp a
>   pp (FromInteger a) = pp a
>   pp (InnerProduct a b) = pp '<' <> pp a <> pp '|' <> pp b <> pp '>'

>instance InnerProductSpace (VectorSpaceExpr a) where
>   x %. y = InnerProduct x y

>instance (Eq a, Show a) => ConjugateSymmetric (NumExpr a) where
>   conj x = x

>instance (Num a, IdVisitor a) => Expression NumExpr [] a where
>   eval = runNumExpr

>instance (Fractional a,IdVisitor a) => Expression FracExpr [] a where
>   eval = runFracExpr
>
>instance (Enum a) => Expression EnumExpr [] a where
>   eval = runEnumExpr

>data FracExpr a = PrimFracExpr (NumExpr a)
>                | Divide (FracExpr a) (FracExpr a)
>                | Recip  (FracExpr a)
>                | FromRational Rational

>instance (PpShow a) => PpShow (FracExpr a) where
>   pp (PrimFracExpr a) = pp a
>   pp (Divide a b) = pp a <> pp '/' <> pp b
>   pp (Recip a) = pp "1/" <> pp a
>   pp (FromRational a) = pp a

>data FloatExpr a = PrimPi
>                 | Exp (FloatExpr a)
>                 | Log (FloatExpr a)
>                 | Sqrt (FloatExpr a)
>                 | Power (FloatExpr a) (FloatExpr a)
>                 | Sin (FloatExpr a)
>                 | Cos (FloatExpr a)
>                 | Tan (FloatExpr a)
>                 | Asin (FloatExpr a)
>                 | Acos (FloatExpr a)
>                 | Atan (FloatExpr a)
>                 | Sinh (FloatExpr a)
>                 | Cosh (FloatExpr a)
>                 | Tanh (FloatExpr a)
>                 | Asinh (FloatExpr a)
>                 | Acosh (FloatExpr a)
>                 | Atanh (FloatExpr a)
>                 | PrimFloatExpr (FracExpr (FloatExpr a))

>pp_unary op a = pp op <> pp '(' <> pp a <> pp ')'

>instance PpShow (FloatExpr a) where
>   pp PrimPi = pp "pi"
>   pp (Exp a) = pp_unary "exp" a
>   pp (Log a) = pp_unary "log" a
>   pp (Sqrt a) = pp_unary "sqrt" a
>   pp (Sin a) = pp_unary "sun" a
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
>   pp (PrimFloatExpr a) = pp a


>instance Show (FloatExpr a) where
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
>   show (PrimFloatExpr a) = show a

>liftNumFrac1 f x = PrimFloatExpr $ PrimFracExpr $ f (NumPrim (Val x))

>liftNumFrac2 op x y = PrimFloatExpr $ PrimFracExpr $ op (NumPrim (Val x))
>                                                        (NumPrim (Val y))

>instance Num (FloatExpr a) where
>   (+) = liftNumFrac2 Plus 
>   (-) = liftNumFrac2 Subtract
>   (*) = liftNumFrac2 Product
>   negate = liftNumFrac1 Negate
>   abs = liftNumFrac1 Abs
>   signum = liftNumFrac1 Signum
>   fromInteger i = PrimFloatExpr $ PrimFracExpr $ FromInteger i

>instance Fractional (FloatExpr a) where
>   x / y = PrimFloatExpr $ Divide (PrimFracExpr (NumPrim (Val x)))
>                                  (PrimFracExpr (NumPrim (Val y)))
>   recip x = PrimFloatExpr $ Recip (PrimFracExpr (NumPrim (Val x)))
>   fromRational x = PrimFloatExpr (FromRational x)

>instance Floating (FloatExpr a) where
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

>data EnumExpr a = SuccExpr (EnumExpr a)
>                | PredExpr (EnumExpr a)
>                | ToEnumExpr (Var a)

>instance (Enum a) => Enum (EnumExpr a) where
>  succ = SuccExpr
>  pred = PredExpr
>  toEnum i = ToEnumExpr $ Val (toEnum i)
>  fromEnum (SuccExpr x) = succ $ fromEnum x
>  fromEnum (PredExpr x) = pred $ fromEnum x
>  fromEnum (ToEnumExpr (Val i)) = fromEnum i
>  fromEnum (ToEnumExpr (Var _ _)) = error "fromEnum: not supported for variables"

>instance Functor FracExpr where
>   fmap f (PrimFracExpr x) = PrimFracExpr (fmap f x)
>   fmap f (Divide x y) = Divide (fmap f x) (fmap f y)
>   fmap f (Recip x) = Recip (fmap f x)
>   fmap f (FromRational r) = FromRational r

>instance (Show a) => Show (FracExpr a) where
>   showsPrec p (PrimFracExpr ne) = showsPrec p ne
>   showsPrec p (Divide x y) = showsBinaryOp 4 "/" p x y
>   showsPrec p (Recip x) = showString "recip(" . showsPrec 0 x . showChar ')'

>instance (Show a) => Show (NumExpr a) where
>   showsPrec p (Plus x y) = showsBinaryOp 3 "+" p x y
>   showsPrec p (Product x y) = showsBinaryOp 4 "*" p x y
>   showsPrec p (Subtract x y) = showsBinaryOp 3 "-" p x y
>   showsPrec p (Negate x)     = showChar '-' . showsPrec 11 x
>   showsPrec p (Abs x)        = showString "abs(" . showsPrec 0 x . showChar ')'
>   showsPrec p (Signum x)     = showString "signum(" . showsPrec 0 x . showChar ')'
>   showsPrec p (NumPrim x)       = showsPrec p x
>   showsPrec p (FromInteger i) = showsPrec p i

>instance Functor NumExpr where
>   fmap f (Plus x y) = Plus (fmap f x) (fmap f y)
>   fmap f (Product x y) = Product (fmap f x) (fmap f y)
>   fmap f (Subtract x y) = Subtract (fmap f x) (fmap f y)
>   fmap f (Negate x) = Negate (fmap f x)
>   fmap f (Abs x) = Abs (fmap f x)
>   fmap f (Signum x) = Signum (fmap f x)
>   fmap f (NumPrim x) = NumPrim (fmap f x)
>   fmap f (FromInteger i) = FromInteger i

>instance Visitor (FracExpr a) where
>  data Fold (FracExpr a) b = FracExprFold {
>             fracexprfold_prim :: Fold (NumExpr a) b,
>             fracexprfold_divide :: b -> b -> b,
>             fracexprfold_recip  :: b -> b,
>             fracexprfold_fromrational :: Rational -> b }
>  visit z (PrimFracExpr p) = visit (fracexprfold_prim z) p
>  visit z (Divide x y) = fracexprfold_divide z (visit z x) (visit z y)
>  visit z (Recip x)    = fracexprfold_recip z (visit z x)
>  visit z (FromRational r) = fracexprfold_fromrational z r

>instance Visitor (FloatExpr a) where
>   data Fold (FloatExpr a) b = FloatExprFold {
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
>     floatfold_prim :: Fold (FracExpr (FloatExpr a)) b }
>   visit z PrimPi = floatfold_pi z
>   visit z (Exp x) = floatfold_exp z (visit z x)
>   visit z (Log x) = floatfold_log z (visit z x)
>   visit z (Sqrt x) = floatfold_sqrt z (visit z x)
>   visit z (Power x y) = floatfold_power z (visit z x) (visit z x)
>   visit z (Sin x) = floatfold_sin z (visit z x)
>   visit z (Cos x) = floatfold_cos z (visit z x)
>   visit z (Tan x) = floatfold_tan z (visit z x)
>   visit z (Asin x) = floatfold_asin z (visit z x)
>   visit z (Acos x) = floatfold_acos z (visit z x)
>   visit z (Atan x) = floatfold_atan z (visit z x)
>   visit z (Sinh x) = floatfold_sinh z (visit z x)
>   visit z (Cosh x) = floatfold_cosh z (visit z x)
>   visit z (Tanh x) = floatfold_tanh z (visit z x)
>   visit z (Asinh x) = floatfold_asinh z (visit z x)
>   visit z (Acosh x) = floatfold_acosh z (visit z x)
>   visit z (Atanh x) = floatfold_atanh z (visit z x)
>   visit z (PrimFloatExpr fr) = visit (floatfold_prim z) fr

>instance Visitor (NumExpr a) where
>  data Fold (NumExpr a) b = NumExprFold { exprfold_plus :: b -> b -> b,
>                                    exprfold_product :: b -> b -> b,
>                                    exprfold_subtract :: b -> b -> b,
>                                    exprfold_negate   :: b -> b,
>                                    exprfold_abs      :: b -> b,
>                                    exprfold_signum   :: b -> b,
>                                    exprfold_prim     :: Fold (Var a) b,
>                                    exprfold_frominteger :: Integer -> b }
>  visit z (Plus x y) = exprfold_plus z (visit z x) (visit z y)
>  visit z (Product x y) = exprfold_product z (visit z x) (visit z y)
>  visit z (Subtract x y) = exprfold_subtract z (visit z x) (visit z y)
>  visit z (Negate x)     = exprfold_negate z (visit z x)
>  visit z (Abs x)        = exprfold_abs z (visit z x)
>  visit z (Signum x)     = exprfold_signum z (visit z x)
>  visit z (NumPrim a)    = visit (exprfold_prim z) a
>  visit z (FromInteger i) = exprfold_frominteger z i

>instance Visitor (EnumExpr a) where
>   data Fold (EnumExpr a) b = EnumExprFold {
>      enumfold_succ :: b -> b,
>      enumfold_pred :: b -> b,
>      enumfold_toenum :: Fold (Var a) b }
>   visit z (SuccExpr x) = enumfold_succ z (visit z x)
>   visit z (PredExpr x) = enumfold_pred z (visit z x)
>   visit z (ToEnumExpr i) = visit (enumfold_toenum z) i

>runEnumExpr :: (Enum a) => [a] -> EnumExpr a -> a
>runEnumExpr ctx = visit (enumExprFold $ varFold ctx)

>runNumExpr :: (Num a, IdVisitor a) => [a] -> NumExpr a -> a
>runNumExpr ctx = visit $ numExprFold (varFold ctx)

>runFracExpr :: (Fractional a, IdVisitor a) => [a] -> FracExpr a -> a
>runFracExpr ctx = visit $ fracExprFold $ numExprFold $ varFold ctx

>varFold :: [a] -> Fold (Var a) a
>varFold ctx = VarFold ctx id

>vecSpaceFold :: (IdVisitor a, VectorSpace b, a ~ Scalar b) =>
>                Fold (Var b) b
>             -> Fold (NumExpr a) (Scalar b)
>             -> Fold a b
>             -> Fold (VectorSpaceExpr a) b
>vecSpaceFold varFold numFold primFold = VectorSpaceExprFold vzero vnegate (%+)
>                          (\e x -> visit numFold e %* x)
>                          (visit varFold) (visit primFold)

>numExprFold :: (Num a) => Fold (Var a) a -> Fold (NumExpr a) a
>numExprFold varFold = NumExprFold (+) (*) (-) negate abs signum
>                                  varFold 
>                                  fromInteger

>fracExprFold :: (Fractional b) =>
>                Fold (NumExpr a) b -> Fold (FracExpr a) b
>fracExprFold numFold = FracExprFold numFold (/) recip fromRational

>enumExprFold :: (Enum b) => Fold (Var a) b -> Fold (EnumExpr a) b
>enumExprFold vFold = EnumExprFold succ pred vFold

>instance Num (NumExpr a) where
>   (+) = Plus
>   (*) = Product
>   (-) = Subtract
>   negate = Negate
>   abs = Abs
>   signum = Signum
>   fromInteger = FromInteger

>instance Num ((NumExpr :*: Var) a) where
>  x + y = Matrix $ Plus (cells x) (cells y)
>  x - y = Matrix $ Subtract (cells x) (cells y)
>  x * y = Matrix $ Product (cells x) (cells y)
>  negate x = Matrix $ Negate (cells x)
>  abs x = Matrix $ Abs (cells x)
>  signum x = Matrix $ Signum (cells x)
>  fromInteger i = Matrix $ FromInteger i


>instance VectorSpace (VectorSpaceExpr a) where
>   type Scalar (VectorSpaceExpr a) = NumExpr a
>   vzero = VZero
>   vnegate = VNegate
>   (%+) = VPlus
>   s %* v = VScalar s v

