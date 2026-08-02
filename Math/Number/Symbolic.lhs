>{-# LANGUAGE GADTs, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies  #-}
>{-# LANGUAGE FlexibleInstances, DefaultSignatures, FlexibleContexts #-}
>{-# LANGUAGE StandaloneDeriving, RankNTypes, UndecidableInstances, TypeOperators #-}
>{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, QuantifiedConstraints, PolyKinds #-}
>module Math.Number.Symbolic where
>import Math.Number.R
>import Math.Number.StreamInterface
>import Math.Number.Stream
>import Data.Typeable
>import Data.List (intersperse)
>import qualified Data.Map as Map
>import qualified Math.Number.Stream as S
>import Math.Matrix.Interface
>import Math.Tools.PrettyP
>import GHC.Base (stimes)
>import Data.Kind
>
>data Name a = Name { name_string :: String, tRep :: Proxy a }
>  deriving (Eq)
>
>instance (Typeable a) => Show (Name a) where
>  show (Name n p) = show n ++ " :: " ++ show (typeRep p)
> 
>newtype Bot = Bot (forall a.a)
>type Map = Map.Map

>data ctx :- a where
>  EFromRational :: ctx :- Rational -> ctx :- R
>  EIntegral :: (ctx :- R, ctx :- R) -> ctx :- (DifferentialForm ctx R -> R -> R) -> ctx :- R
>  ELambda :: (Typeable a) => Name a -> (ctx,a) :- b -> ctx :- (a -> b)
>  EVar    :: (Typeable a, ContextLookup (Context ctx) a) => Name a -> ctx :- a
>  EApp    :: ctx :- (a -> b) -> ctx :- a -> ctx :- b
>  EAdd    :: (Num a) => ctx :- a -> ctx :- a -> ctx :- a
>  EMul    :: (Num a) => ctx :- a -> ctx :- a -> ctx :- a
>  ENegate :: (Num a) => ctx :- a -> ctx :- a
>  ELess :: ctx :- R -> ctx :- R -> ctx :- Bool
>  ETerminal :: ctx :- ()
>  EProduct :: ctx :- a -> ctx :- b -> ctx :- (a,b)
>  EFst :: ctx :- (a,b) -> ctx :- a
>  ESnd :: ctx :- (a,b) -> ctx :- b
>  EInitial :: Bot :- a
>  ECase :: ctx :- (a -> c) -> ctx :- (b -> c) -> ctx :- (Either a b -> c)
>  EIn1  :: ctx :- (a -> Either a b)
>  EIn2  :: ctx :- (b -> Either a b)
>  ERationalConstant :: Rational -> ctx :- Rational
>  ERealConstant :: R -> ctx :- R
>  EIntegerConstant :: Integer -> ctx :- Integer
>  EIntConstant :: Int -> ctx :- Int
>  EPolynomial :: Poly (ctx :- a) -> ctx :- (Poly a)
>  EEvalPolynomial :: ctx :- (Poly a) -> ctx :- (a -> a)

>eid :: (Typeable b, ContextLookup (Context ctx) b)
>   => Name b -> ctx :- (b -> b)
>eid x = ELambda x (EVar x)

>ecompose :: (Typeable a,Typeable b, Typeable c,
> ContextLookup (Context ctx) a,
> ContextLookup (Context ctx) (a -> b),
> ContextLookup (Context ctx) (b -> c))
>         => Name (b -> c) -> Name (a -> b) -> Name a
>         -> ctx :- ((b -> c) -> (a -> b) -> a -> c)
>ecompose f g x = ELambda f $ ELambda g $ ELambda x $
>     EApp (EVar f) (EApp (EVar g) (EVar x))


>class ContextLookup ctx a where
>  ctx_lookup :: ctx -> Name a -> Maybe a

>data Context a where
>  MakeCtx :: (Typeable a) => Context ctx -> Name a -> a -> Context (ctx,a)
>  EmptyCtx :: Context ()

>instance (ContextLookup (Context ctx) a, Typeable a) => ContextLookup (Context (ctx,b)) a where
>  ctx_lookup (MakeCtx c (Name n nrep) v) z@(Name n' nrep')
>   | n == n' = if typeRep nrep == typeRep nrep' then cast v else
>         error $ "type mismatch during context lookup of '" ++ n ++ "' :"
>              ++ showsTypeRep (typeRep nrep) [] ++ " != " ++ showsTypeRep (typeRep nrep') []
>   | otherwise = ctx_lookup c z

>instance ContextLookup (Context ()) () where
>  ctx_lookup _ n' = Nothing

>type EvalContext ctx = forall a. ContextLookup (Context ctx) a

>type ctx := a = Context ctx -> a

>evalExpr :: ctx :- a -> ctx := a
>evalExpr z@(EVar n) c | Just v <- ctx_lookup c n = v
>                      | otherwise = error $ "Cannot find variable:" ++ show n
>evalExpr (EFromRational e) c = fromRational $ evalExpr e c
>evalExpr (EApp (ELambda n f) x) c = evalExpr f (MakeCtx c n (evalExpr x c))
>evalExpr (EAdd a b) c = evalExpr a c + evalExpr b c
>evalExpr (EMul a b) c = evalExpr a c * evalExpr b c
>evalExpr (ENegate a) c = negate (evalExpr a c)
>evalExpr ETerminal _ = ()
>evalExpr (EProduct a b) c = (evalExpr a c, evalExpr b c)
>evalExpr (EFst p) c = fst (evalExpr p c)
>evalExpr (ESnd p) c = snd (evalExpr p c)
>evalExpr (ECase f g) c = Prelude.either (evalExpr f c) (evalExpr g c)
>evalExpr EIn1 c = Left
>evalExpr EIn2 c = Right
>evalExpr (ERationalConstant q) c = q
>evalExpr (ERealConstant r) c = r
>evalExpr (EIntegerConstant z) c = z
>evalExpr (EIntConstant i) c = i
>evalExpr (EPolynomial p) c = fmap (`evalExpr` c) p
>-- evalExpr (EEvalPolynomial p) c = evalPoly (evalExpr p c)

>precedence :: ctx :- a -> Int
>precedence ETerminal = 10
>precedence (EFromRational {}) = 10
>precedence (EVar {}) = 10
>precedence (ERationalConstant {}) = 10
>precedence (ERealConstant {}) = 10
>precedence (EIntegerConstant {}) = 10
>precedence (EIntConstant {}) = 10
>precedence (EFst {}) = 9
>precedence (ESnd {}) = 9
>precedence EIn1 = 9
>precedence EIn2 = 9
>precedence EInitial = 9
>precedence (EMul {}) = 8
>precedence (ENegate {}) = 7
>precedence (EAdd {}) = 6
>precedence (ELess {}) = 4
>precedence (EIntegral {}) = 2
>precedence (EPolynomial {}) = 1
>precedence (EEvalPolynomial {}) = 1
>precedence (EProduct {}) = 1
>precedence (ECase {}) = 1
>precedence (ELambda {}) = 0
>precedence (EApp {}) = 0

>instance Num (ctx :- Integer) where
>  a + b = EAdd a b
>  a - b = EAdd a (ENegate b)
>  a * b = EMul a b
>  negate = ENegate
>  abs x = error "not supported: abs"
>  signum x = error "not supported: signum"
>  fromInteger x = EIntegerConstant x

>instance Show (ctx :- a) where
>  showsPrec i e
>    | i >= precedence e = showChar '(' . showsPrec 0 e . showChar ')'
>    | otherwise = case e of
>        (EFromRational x) -> showString "fromRational(" . showsPrec 10 x . showString ")"
>        (ERationalConstant r) -> shows r
>        (ERealConstant r) -> shows r
>        (EIntegerConstant r) -> shows r
>        (EIntConstant r) -> shows r
>        ETerminal -> showString "()"
>        EInitial  -> showString "undefined"
>        EIn1 -> showString "in1"
>        EIn2 -> showString "in2"
>        (EFst p) -> showString "fst(" . showsPrec 10 p . showChar ')'
>        (ESnd p) -> showString "snd(" . showsPrec 10 p . showChar ')'
>        (EIntegral (a,b) f) -> showString "integral["
>                         . showsPrec 9 a . showString "," . showsPrec 9 b . showString "] "
>                         . showsPrec 2 f
>        (ELambda n f) -> showString "\\ " . showsPrec 9 n . showString " -> " . showsPrec 5 f
>        (EVar (Name n _)) -> showString n
>        (EApp f x) -> showsPrec 9 f . showChar ' ' . showsPrec 9 x
>        (EMul x y) -> showsPrec 8 x . showChar '*' . showsPrec 8 y
>        (ENegate x) -> showChar '-' . showsPrec 7 x
>        (EAdd x y) -> showsPrec 6 x . showChar '+' . showsPrec 6 y
>        (ELess a b) -> showsPrec 4 a . showChar '<' . showsPrec 4 b
>        (EProduct a b) -> showChar '(' . showsPrec 1 a . showChar ',' . showsPrec 1 b . showChar ')'
>        (EPolynomial p) -> showsPrec 1 p
>        (EEvalPolynomial p) -> showsPrec 0 p
>        (ECase f g) -> showChar '[' . showsPrec 0 f . showChar ',' . showsPrec 0 g . showChar ']'

>appPoly :: Poly (ctx :- a) -> ctx :- a -> ctx :- a
>appPoly = EApp . EEvalPolynomial . EPolynomial

>data DifferentialForm ctx a where
>  DF_dX :: Rational -> DifferentialForm ctx R
>  Derivative_Times_dX :: { df_derivative :: ctx :- (R -> R),
>                          df_form :: DifferentialForm ctx R } -> DifferentialForm ctx R

>data Coeff a = Coeff { pcoeff :: a, pindex :: Integer }
>newtype Poly a = Poly { coefficients :: [Coeff a] }

>instance Functor Coeff where
>   fmap f (Coeff x i) = Coeff (f x) i

>instance Functor Poly where
>   fmap f (Poly lst) = Poly $ map (fmap f) lst

>instance Show (Coeff Integer) where
>  show (Coeff c 0) = show c
>  show (Coeff 1 1) = show "x"
>  show (Coeff c 1) = show c ++ "x"
>  show (Coeff 1 i) = "x^" ++ show i
>  show (Coeff c i) = show c ++ "x^" ++ show i
>instance Show (Coeff Rational) where
>  show (Coeff c 0) = show c
>  show (Coeff 1 1) = show "x"
>  show (Coeff c 1) = show c ++ "x"
>  show (Coeff 1 i) = "x^" ++ show i
>  show (Coeff c i) = show c ++ "x^" ++ show i
>instance Show (Coeff Double) where
>  show (Coeff c 0) = show c
>  show (Coeff c 1) = show c ++ "x"
>  show (Coeff c i) = show c ++ "x^" ++ show i
>instance Show (Coeff Float) where
>  show (Coeff c 0) = show c
>  show (Coeff c 1) = show c ++ "x"
>  show (Coeff c i) = show c ++ "x^" ++ show i
>instance Show (Coeff (ctx :- a)) where
>  show (Coeff c 0) = show c
>  show (Coeff c 1) = show c ++ "x"
>  show (Coeff c i) = show c ++ "x^" ++ show i

>instance (Show (Coeff a)) => Show (Poly a) where
>  show (Poly lst) = concat $ intersperse " + " [ show c | c <- reverse lst ]

>instance (Num a) => Monoid (Coeff a) where
>   mempty = Coeff 1 0
> 
>instance (Num a) => Semigroup (Coeff a) where
>   (Coeff a i) <> (Coeff b j) = Coeff (a * b) (i + j)
> 

>instance (Num a) => Monoid (Poly a) where
>   mempty = Poly []

>instance (Num a) => Semigroup (Poly a) where
>   (Poly lst) <> (Poly lst') = polySum (lst ++ lst')

>insertCoeff :: (a -> a -> a) -> Coeff a -> [Coeff a] -> [Coeff a]
>insertCoeff _ z [] = [z]
>insertCoeff f z1@(Coeff c i) (z2@(Coeff d j): r)
>   | i < j = z1 : insertCoeff f z2 r
>   | i > j = z2 : insertCoeff f z1 r
>   | otherwise = insertCoeff f (Coeff (f c d) i) r

>reduceCoefficients :: (Num a) => [Coeff a] -> [Coeff a]
>reduceCoefficients (cr@(Coeff c i) : r1@(dr@(Coeff d j) : r))
>  | i < j = insertCoeff (+) cr (reduceCoefficients r1)
>  | i > j = insertCoeff (+) dr (reduceCoefficients (cr : r))
>  | i == j = insertCoeff (+) (Coeff (c + d) i) (reduceCoefficients r)
>reduceCoefficients [x] = [x]
>reduceCoefficients [] = []

>polySum :: (Num a) => [Coeff a] -> Poly a
>polySum = Poly . reduceCoefficients

>polyTimes :: (Num a) => Poly a -> Poly a -> Poly a
>polyTimes (Poly lst) (Poly lst') = polySum [ x <> y | x <- lst, y <- lst' ]

>evalCoeff :: (Semigroup v, VectorSpace v) => Coeff (Scalar v) -> v -> v
>evalCoeff (Coeff c i) x = c %* stimes i x

>evalPoly :: (Semigroup v, VectorSpace v) => Poly (Scalar v) -> v -> v
>evalPoly (Poly lst) x = vsum [ evalCoeff c x | c <- lst]

>instance (VectorSpace a, Num a) => Num (Poly a) where
>  a + b = a <> b
>  a - b = a <> vnegate b
>  (*) = polyTimes
>  negate = vnegate
>  abs (Poly lst) = Poly [ Coeff (abs x) i | Coeff x i <- lst ]
>  signum (Poly lst) = Poly [ Coeff (signum x) i | Coeff x i <- lst ]
>  fromInteger = pconstant . fromInteger

>class Polynomial p a where
>  pconstant :: a -> p a
>  pXPower :: Integer -> p a

>pX :: (Num a) => Poly a
>pX = pXPower 1

>instance (Num a) => VectorSpace (Poly a) where
>  type Scalar (Poly a) = a
>  vzero = Poly []
>  vnegate (Poly lst) = Poly [ Coeff (negate x) i | Coeff x i <- lst ]
>  (Poly x) %+ (Poly y) = polySum (x ++ y)
>  k %* (Poly lst) = Poly [ Coeff (k * x) i | Coeff x i <- lst ]

>instance (Num a) => Polynomial Coeff a where
>   pconstant x = Coeff x 0
>   pXPower i = Coeff 1 i

>instance (Num a) => Polynomial Poly a where
>   pconstant x = Poly [pconstant x]
>   pXPower i = Poly [pXPower i]


