>-- -*- mode: haskell ; coding: utf-8 -*-
>{-# LANGUAGE Safe, GADTs, TypeFamilies, FlexibleContexts #-}
>{-# LANGUAGE FlexibleInstances, TypeOperators, DataKinds #-}
>{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, DeriveGeneric #-}
>{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
>-- | This module provides dimensional analysis according to SI system of units.
>--   For reference have used <https://en.wikipedia.org/wiki/Dimensional_analysis>
>--   and <https://en.wikipedia.org/wiki/International_System_of_Units>.
>--
>--  This module supports run-time checked quantities.
>--  
>--  In reality this should be according to the International system of units, but
>--  I have not used the official standard documents when writing this code.
>--  However it is recognized that any major deviation from
>--  the standard would be considered a bug in this code.
>--  And definitely the International system of units is basis for this code.
>-- 
>-- For official references, see e.g. "ISO80000-1:2009:Quantities and Units" and
>--           "NIST Special Publication 330, 2008: The International system of units".
>--
>-- For c++ approach to dimensional analysis, see
>-- "Barton&Nackman: Scientific and Engineering C++".
>-- 
>-- Example uses:
>-- 
>-- @show (3 %* meter) = "3 m"@
>--
>-- @3 %* meter + 4 %* kilogram == throw (InvalidDimensionsException meterDimension kilogramDimension "<...>")@
>-- 
>-- @convert (3 %* meter) (milli meter) == return 3000.0@
>--
>-- @convert (3 %* meter) kilogram == fail "..."@
>--
>-- @(3 %* meter) =/ (milli meter) == 3000.0@
>--
>-- @(3 %* meter) =/ kilogram == error "..."@
>--
>-- @convert lightyear (kilo meter) == return 9.4607304725808e12@
>--
>-- @3 `logarithmic` dBV == 1000.0 %* volt@
>module Math.Number.DimensionalAnalysis where
>import safe qualified Text.PrettyPrint as Pretty (Doc,vcat,nest, (<+>), empty) 
>import safe Data.Complex
>import safe Data.Typeable
>import safe GHC.Generics
>import safe Data.Data
>import safe Data.Ratio
>import safe Data.Char
>import safe qualified Data.Binary as Binary
>import safe Control.Monad (guard, mplus, foldM)
>import safe Control.Applicative (Alternative(many,some,(<|>)))
>import safe Control.Exception
>import Math.Tools.PrettyP hiding (parens)
>import Math.Tools.Functor (interleave)
>import Math.Tools.List ()
>import Math.Tools.Show
>import Math.Matrix.Interface
>import Math.Number.StreamInterface (Stream(..), Limiting(..), Closed(..), Infinitesimal(..))
>import qualified Math.Number.StreamInterface as Stream
>import qualified Math.Number.Stream
>import Math.Number.Interface ( ShowPrecision(..), RationalRoots(..))
>import Math.Number.Group
>import Math.Number.Interface
>import Math.Tools.Isomorphism
>import safe qualified Text.ParserCombinators.ReadPrec as ReadPrec
>import safe qualified Text.Read as TRead
>import safe qualified Math.Matrix.Covector as Covector

>data Quantity r = As {
>   valueAmount :: r,
>   valueDimension :: !Dimension }
>  deriving (Typeable, Data, Generic)


>instance Applicative Quantity where
>   pure x = x `As` dimensionless
>   (f `As` d) <*> (x `As` d') = (f x) `As` (d %+ d')

>instance (DedekindCut a b, Show a, Num a) => DedekindCut (Quantity a) (Quantity b) where
>  (r `As` d') %< (x `As` d)
>     | d == d' = r %< x
>     | otherwise = invalidDimensions "%<" d' d "" ""
>  (x `As` d) <% (r `As` d')
>     | d == d' = x <% r
>     | otherwise = invalidDimensions "<%" d d' "" ""


>instance DedekindCut Double (Quantity Double) where
>  r %< (x `As` d)
>     | isDimensionless d = r %< x
>     | otherwise = invalidDimensions "%<" dimensionless d r x
>  (x `As` d) <% r
>     | isDimensionless d = x <% r
>     | otherwise = invalidDimensions "<%" d dimensionless x r

>instance DedekindCut Float (Quantity Float) where
>  r %< (x `As` d)
>     | isDimensionless d = r %< x
>     | otherwise = invalidDimensions "%<" dimensionless d r x
>  (x `As` d) <% r
>     | isDimensionless d = x <% r
>     | otherwise = invalidDimensions "<%" d dimensionless x r


>-- | checked projection
>dimensionlessAmount :: (Show r) => Quantity r -> r
>dimensionlessAmount (x `As` d) | isDimensionless d = x
>                             | otherwise = invalidDimensions "dimensionlessAmount" d dimensionless x x

>instance (Show r, DifferentiallyClosed r, VectorSpace r) => DifferentiallyClosed (Quantity r) where
>  derivate f (x `As` d) = derivate (\x' -> valueAmount $ f (x' `As` d)) x
>                     `As` (dimension (f (x `As` d)) - d)
>  integral (a `As` da,b `As` db) f
>     | da == db =
>         integral (a,b) (\x -> valueAmount (f (x `As` da)))
>         `As` ((dimension $! f (a `As` da)) %+ da)
>     | otherwise = error $ "integral: invalid dimensions: " ++ show da ++ " != " ++ show db

>instance (Show r, Ord r) => Eq (Quantity r) where
>   (x `As` d) == (y `As` d') | d == d' = x == y
>                       | otherwise = invalidDimensions "==" d d' x y

>instance (Show (Closure Stream r), Eq (Closure Stream r)) => Eq (Closure Stream (Quantity r)) where
>   (QuantityClosure x d) == (QuantityClosure y d')
>     | d == d' = x == y
>     | otherwise = invalidDimensions "==-closure" d d' x y

>equalUpTo :: (Floating a, Show a, Ord a) => a -> Quantity a -> Quantity a -> Bool
>equalUpTo tolerance (x `As` d) (y `As` d')
>   | d == d' = abs (x-y) < tolerance
>   | otherwise = invalidDimensions "equalUpTo" d d' x y

>data Dimension = Dimension {
>   lengthPower :: {-# UNPACK #-} !Rational,
>   weightPower :: {-# UNPACK #-} !Rational,
>   timePower   :: {-# UNPACK #-} !Rational,
>   currentPower :: {-# UNPACK #-} !Rational,
>   temperaturePower :: {-# UNPACK #-} !Rational,
>   luminosityPower  :: {-# UNPACK #-} !Rational,
>   substancePower   :: {-# UNPACK #-} !Rational }
>  deriving (Eq, Typeable, Ord, Data, Generic)

>instance ShowPrecision Dimension where
>  showAtPrecision r _ = show r

>instance StandardBasis Dimension where
>  unitVectors = [meterDimension,kilogramDimension,secondDimension,
>                   ampereDimension, kelvinDimension, candelaDimension,
>                   molDimension]

>dimensionMultiples :: Dimension -> Stream Dimension
>dimensionMultiples d = Pre d $ fmap (%+ d) $ dimensionMultiples d

>quantityPowers :: (Num a, Show a) => Quantity a -> Stream (Quantity a)
>quantityPowers u = Stream.iterateStream (* u) u

>-- | isFractionalDimensional checks if a dimension is fractional dimensional
>isFractionalDimensional :: Dimension -> Bool
>isFractionalDimensional (Dimension l w t c te lu su)
>  = denominator l /= 1 || denominator w /= 1 || denominator t /= 1 ||
>    denominator c /= 1 || denominator te /= 1 || denominator lu /= 1 ||
>    denominator su /= 1

>instance (Binary.Binary r) => Binary.Binary (Quantity r) where
>   put (x `As` d) = Binary.put x >> Binary.put d
>   get = do { x <- Binary.get ; d <- Binary.get ; return (x `As` d) }

>type Prefix r = Quantity r -> Quantity r

>hasDimension :: Quantity r -> Dimension -> Bool
>hasDimension (_ `As` d) d' = d == d'

>instance Functor Quantity where
>   fmap f = \ (x `As` d) -> f x `As` d

>data DimensionException = InvalidDimensionsException Dimension Dimension String
>  deriving (Typeable, Show)

>instance Exception DimensionException

>instance (Show r, Infinitesimal Stream r) => Infinitesimal Stream (Quantity r) where
>   epsilonClosure = QuantityClosure epsilonClosure dimensionless
>   infiniteClosure = QuantityClosure infiniteClosure dimensionless
>   minusInfiniteClosure = QuantityClosure minusInfiniteClosure dimensionless
>   nanClosure = QuantityClosure nanClosure dimensionless
>   epsilonStream = fmap (`As` dimensionless) epsilonStream
>   infiniteStream = fmap (`As` dimensionless) infiniteStream
>   minusInfiniteStream = fmap (`As` dimensionless) minusInfiniteStream

>instance (Show r, InnerProductSpace r, Scalar (Quantity r) ~ Scalar r) => InnerProductSpace (Quantity r) where
>   (x `As` d) %. (y `As` d') = x %. y

>mapQuantity :: (a -> b)
>            -> (Dimension -> Dimension)
>            -> Quantity a -> Quantity b
>mapQuantity f g = \ ~(r `As` d) -> f r `As` g d

>mapQuantity2 :: (a -> b -> c) -> (Dimension -> Dimension -> Dimension)
>             -> Quantity a -> Quantity b -> Quantity c
>mapQuantity2 f g ~(x `As` d) ~(y `As` d') = f x y `As` g d d'

>complexQuantity :: (Show r) => Complex (Quantity r) -> Quantity (Complex r)
>complexQuantity ~(~(a `As` d) :+ ~(b `As` d'))
>   | d == d' = (a :+ b) `As` d
>   | otherwise = invalidDimensions "complexQuantity" d d' a b

>quantityComplex :: Quantity (Complex r) -> Complex (Quantity r)
>quantityComplex ~(~(a :+ b) `As` d) = (a `As` d) :+ (b `As` d)

>ratioQuantity :: (Integral r, Show r) => Ratio (Quantity r) -> Quantity (Ratio r)
>ratioQuantity r = ((a % b) `As` (d - d'))
>    where (a `As` d) = numerator  r
>          (b `As` d') = denominator r

>-- | the Unit class should be defined by any newtype based
>-- types that should interact well with the dimensional analysis mechanism.
>class (VectorSpace u) => Unit u where
>   amount :: u -> Scalar u
>   fromQuantity :: (Alternative m, MonadFail m) => Quantity (Scalar u) -> m u
>   unitOf :: u -> String
>   dimension :: u -> Dimension

>-- | The fromAmount method must check that compile-time information about dimensions
>-- is sufficient to determine dimension of the given input
>-- e.g. @(fromAmount 3 :: Mass)@ is ok, but @(fromAmount 3 :: Quantity Double)@ is not.
>class (Unit u) => LiteralUnit u where
>   fromAmount :: Scalar u -> u
>   zeroAmount :: (Scalar u -> u) -> Scalar u
>   conversionFactor :: (Scalar u -> u) -> Scalar u
>   zeroAmount _ = 0
>   conversionFactor _ = 1

>fromQuantityDef :: (MonadFail m, Alternative m, Show a) => Dimension -> (a -> b) -> Quantity a -> m b
>fromQuantityDef dim ctr = \ (x `As` d) -> do { guard (d == dim) ; return $! ctr x }
>                                <|> invalidDimensionsM "fromQuantityDef" d dim x x

>instance (Closed a, Show a) => Closed (Quantity a) where
>   accumulationPoint s
>     | (Stream.Pre (As x d) xr) <- approximations s
>     = accumulationPoint (limit $ Stream.Pre x (fmap valueAmount xr)) `As` d

>-- | <https://en.wikipedia.org/wiki/Level_(logarithmic_quantity)
>-- Level represents a reference to which computations involving
>-- logarithmic scales are compared to.
>data Level r = Level {
>   referenceValue :: Quantity r,
>   baseOfLogarithm :: r }

>deriving instance Read (Level Double)
>deriving instance Read (Level (Complex Double))
>deriving instance Read (Level Integer)
>deriving instance Read (Level Int)
>deriving instance Show (Level Double)
>deriving instance Show (Level (Complex Double))
>deriving instance Show (Level Integer)
>deriving instance Show (Level Int)

>scale :: (Show a, Floating a, Real a) => Quantity a -> Level a -> Quantity a
>scale x (Level q b) = log (x / q) / log (pure b)

>logarithmic :: (Floating a) => a -> Level a -> Quantity a
>logarithmic x (Level q b) = (b ** x) %* q

>hyperbolic :: (RealFloat a) => Complex a -> Level (Complex a) -> Quantity (Complex a)
>hyperbolic x (Level q b) = (b ** ((0 :+ 1) * x)) %* q

>-- | This is a way to convert radians to complex numbers
>-- e.g. @(pi/2) `logarithmic` radianScale == 0 :+ 1@.
>radianScale :: (RealFloat a) => Level (Complex a)
>radianScale = Level (pure (1 :+ 0)) (exp (0 :+ 1))

>bitScale :: (Num a) => Level a
>bitScale = Level (1 `As` dimensionless) 2

>belScale :: (Num a) => Level a
>belScale = Level (1 `As` dimensionless) 10

>-- | logarithmic voltage with respect to base 10 relative to 1V.
>-- <https://en.wikipedia.org/wiki/Decibel>
>dBV :: (Floating a) => Level a
>dBV = Level (1 %* volt) 10

>-- | <https://en.wikipedia.org/wiki/Decibel>
>-- logarithmic voltage in base 10 relative to \(\sqrt{0.6}V\).
>dBu :: (Floating a) => Level a
>dBu = Level ((sqrt(0.6)) %* volt) 10

>-- | <https://en.wikipedia.org/wiki/Decibel>
>-- logarithmic pressure in base 10 compared to 20 micro pascals.
>dBSPL :: (Floating a) => Level a
>dBSPL = Level (20 %* micro pascal) 10

>-- | <https://en.wikipedia.org/wiki/Decibel>
>dBSIL :: (Floating a, Show a) => Level a
>dBSIL = Level ((10**(negate 12)) %* (watt / squaremeter)) 10

>-- <https://en.wikipedia.org/wiki/Decibel>
>dBSWL :: (Floating a) => Level a
>dBSWL = Level (10e-12 %* watt) 10

>-- | for logarithmic lengths in base 10.
>lengthScale :: (Floating a) => Level a
>lengthScale = Level (1.0 %* meter) 10

>-- | <https://en.wikipedia.org/wiki/Octave>
>-- logarithmic frequency in base 2 relative to 16.352 Hz.
>-- a.k.a. in scientific designation.
>-- middle-C is fourth octave, e.g. @4 \`logarithmic\` octave == 261.626 %* hertz@
>frequencyScale :: (Floating a) => Level a
>frequencyScale = Level (16.352 %* hertz) 2

>-- | <https://en.wikipedia.org/wiki/Octave>
>-- logarithmic frequency in base 2 relative to 16.352 Hz.
>-- a.k.a. in scientific designation.
>-- middle-C is fourth octave, e.g. @4 \`logarithmic\` octave == 261.626 %* hertz@
>-- octave is just a different name for 'frequencyScale'.
>octave :: (Floating a) => Level a
>octave = frequencyScale

>-- | <https://en.wikipedia.org/wiki/Moment_magnitude_scale>
>momentMagnitudeScale :: (Show a, Floating a) => Level a
>momentMagnitudeScale = Level ((10 ** (-7)) %* (newton * meter)) 10

>-- | <https://en.wikipedia.org/wiki/Entropy>
>entropyScale :: (Floating a) => Level a
>entropyScale = Level boltzmannConstant (exp 1)

>hartleyScale :: (Num a) => Level a
>hartleyScale = Level (1 `As` dimensionless) 10

>banScale :: (Num a) => Level a
>banScale = hartleyScale

>timeScale :: (Floating a) => Level a
>timeScale = Level (1 %* second) 10

>weightScale :: (Floating a) => Level a
>weightScale = Level (1 %* kilogram) 10

>-- | compute how many nines a probability has. @nines 0.99999 == 5.0@.
>-- <https://en.wikipedia.org/wiki/9#Probability>
>nines :: Double -> Double
>nines prob = valueAmount $ negate $ (1.0 `As` dimensionless - prob `As` dimensionless) `scale` belScale

>-- | compute probability when given number of nines.
>-- <https://en.wikipedia.org/wiki/9#Probability>
>fromNines :: Double -> Double
>fromNines n = valueAmount $ 1.0 - ((negate n) `logarithmic` Level (1.0 `As` dimensionless) 10)

>-- | note frequency from octave and index of note within octave.
>-- @notePitch 4 0@ is middle-C.
>notePitch :: Int -> Int -> Quantity Double
>notePitch oct noteIndex = (fromIntegral oct + fromIntegral noteIndex / 12) `logarithmic` frequencyScale

>ratioToDecibel :: (Fractional a, Show a, Floating a, Real a) => Quantity a -> Level a -> Quantity a
>ratioToDecibel x lvl = baseOfLogarithm lvl %* (x `scale` lvl)

>-- | WARNING: native representation is in number of bits.
>-- Use code such as the following for conversion:
>-- @gibi byte \`convert\` kilo bit == return 8589934.592@
>bit :: (Floating a, Real a, Show a) => Quantity a
>bit = fromAlternatives 2

>-- | Byte as the number of bits.
>-- WARNING: native representation is in number of bits.
>-- use code such as the following for conversion:
>-- @gibi byte \`convert\` kilo bit == return 8589934.592@
>byte :: (Floating a, Real a, Show a) => Quantity a
>byte = fromAlternatives 256

>fromAlternatives :: (Floating a, Real a, Show a) => a -> Quantity a
>fromAlternatives count = scale (count `As` dimensionless) bitScale

>-- | Note that the number of alternatives grows quickly.
>toAlternatives :: (Floating a, Ord a, ShowPrecision a, MonadFail m) => Quantity a -> m a
>toAlternatives x
>   | isDimensionless (valueDimension x) = return $! 2.0 ** valueAmount x
>   | otherwise = invalidDimensionsM "toAlternatives" (valueDimension x) dimensionless x x

>(@@) :: r -> Dimension -> Quantity r
>x @@ y = x `As` y

>convertTo :: (MonadFail m, Floating a, ShowPrecision a,Ord a, VectorSpace a)
>  => Quantity a -> (String, Quantity a) -> m String
>convertTo value (qname,base) = do
>   val <- convert value base
>   return $! show val ++ qname

>instance (Num r, VectorSpace r) => Unit (Quantity r) where
>  amount (x `As` _) = x
>  unitOf (_ `As` r) = show r
>  fromQuantity q = return $! q
>  dimension (_ `As` r) = r
>  -- | fromAmount is not implemented, as the dimension is not known.
  
>(=/) :: (Fractional (Scalar a), Unit a, Show a) => a -> a -> Scalar a
>(=/) x y
>   | dimension x == dimension y = amount x / amount y
>   | otherwise = invalidDimensions "(=/)" (dimension x) (dimension y) x y

>-- | conversions between units. Dimensions have to match.
>convert :: (Scalar u ~ Scalar v, Unit v, Unit u, Show v, Show u, MonadFail m, Fractional (Scalar u))
>        => v -> u -> m (Scalar u)
>convert x d
>   | dimension x == dimension d = return $! (amount x / amount d)
>   | otherwise = invalidDimensionsM "convert" (dimension x) (dimension d) x d


>instance Binary.Binary Dimension where
>   put (Dimension l w t c te lu su) = do
>     Binary.put l >> Binary.put w >> Binary.put t >> Binary.put c
>     >> Binary.put te >> Binary.put lu >> Binary.put su
>   get = do
>     l <- Binary.get
>     w <- Binary.get
>     t <- Binary.get
>     c <- Binary.get
>     te <- Binary.get
>     lu <- Binary.get
>     su <- Binary.get
>     return $! Dimension l w t c te lu su

>dimensionBasis :: [Dimension]
>dimensionBasis = [meterDimension, kilogramDimension, secondDimension,
>                   ampereDimension, kelvinDimension, candelaDimension,
>                   molDimension]

>isDimensionless :: Dimension -> Bool
>isDimensionless d = d == dimensionless

>mapDimension :: (Rational -> Rational) -> Dimension -> Dimension
>mapDimension f (Dimension l w t c te lu su) = Dimension (f l) (f w) (f t) (f c) (f te) (f lu) (f su) 

>zipWithDimension :: (Rational -> Rational -> Rational) -> Dimension -> Dimension -> Dimension
>zipWithDimension f (Dimension l w t c te lu su) (Dimension l' w' t' c' te' lu' su') = Dimension (f l l') (f w w') (f t t') (f c c') (f te te') (f lu lu') (f su su')

>instance Num Dimension where
>   a + b = a %+ b
>   a - b = a %+ (vnegate b)
>   a * b = zipWithDimension (*) a b
>   negate x = dimensionless - x
>   abs a = mapDimension abs a
>   signum a = mapDimension signum a
>   fromInteger i = Dimension (fromInteger i) (fromInteger i) (fromInteger i) (fromInteger i) (fromInteger i) (fromInteger i) (fromInteger i) 

>instance Fractional Dimension where
>   a / b = zipWithDimension (/) a b
>   recip a = fromInteger 1 / a
>   fromRational i = Dimension i i i i i i i 

>instance InnerProductSpace Dimension where
>   (Dimension l w t c te lu su) %. (Dimension l' w' t' c' te' lu' su')
>     = l*l'+w*w'+t*t'+c*c'+te*te'+lu*lu'+su*su'

>instance Semigroup Dimension where
>   (<>) = (%+)

>instance Monoid Dimension where
>   mempty = dimensionless
>   mappend = (<>)

>instance Group Dimension where
>   ginvert = vnegate


>instance VectorSpace Dimension where
>   type Scalar Dimension = Rational
>   vzero = dimensionless
>   vnegate a = mapDimension negate a
>   a %+ b = zipWithDimension (+) a b
>   k %* a = mapDimension (k *) a

>instance Unit Dimension where
>   amount _ = 1
>   fromQuantity q = return (valueDimension q)
>   unitOf = showDimension
>   dimension = id

>(=*) :: (VectorSpace v) => Scalar v -> v -> v
>(=*) = (%*)

>-- | <https://en.wikipedia.org/wiki/Metric_prefix>
>deca,hecto,kilo,mega,giga,tera,peta,exa,zetta,yotta,ronna,quetta :: (VectorSpace u, Num (Scalar u)) => u -> u
>deca = (10 =*)
>hecto = (100 =*)
>kilo = (1000 =*)
>mega = (1000000 =*)
>giga = (1000000000 =*)
>tera = (1000000000000 =*)
>peta = (1000000000000000 =*)
>exa  = (1000000000000000000 =*)
>zetta = (1000000000000000000000 =*)
>yotta = (1000000000000000000000000 =*)
>ronna = (1000000000000000000000000000 =*)
>quetta =(1000000000000000000000000000000 =*)

>prefixValue :: (Num a) => Prefix a -> a
>prefixValue f = valueAmount $ f $ 1 `As` dimensionless

>-- | <https://en.wikipedia.org/wiki/Metric_prefix>
>decimalPrefixes :: (Num a) => [(String,a)]
>decimalPrefixes = [
>   ("deca",prefixValue deca),
>   ("hecto",prefixValue hecto),
>   ("kilo",prefixValue kilo),
>   ("mega",prefixValue mega),
>   ("giga",prefixValue giga),
>   ("tera",prefixValue tera),
>   ("peta",prefixValue peta),
>   ("exa", prefixValue exa),
>   ("zetta", prefixValue zetta),
>   ("yotta", prefixValue yotta),
>   ("ronna", prefixValue ronna),
>   ("quetta", prefixValue quetta)]

>-- | <https://en.wikipedia.org/wiki/Metric_prefix>
>floatingPrefixes :: (Floating a) => [(String,a)]
>floatingPrefixes = decimalPrefixes ++ [ 
>   ("deci",prefixValue deci ),
>   ("centi",prefixValue centi ),
>   ("milli",prefixValue milli),
>   ("micro",prefixValue micro),
>   ("nano",prefixValue nano ),
>   ("pico",prefixValue pico ),
>   ("femto",prefixValue femto ),
>   ("atto", prefixValue atto),
>   ("zepto",prefixValue zepto ),
>   ("yocto",prefixValue yocto),
>   ("ronto",prefixValue ronto),
>   ("quecto",prefixValue quecto)]
>                    
>binaryPrefixes :: (Num a) => [(String,a)]
>binaryPrefixes = [
>   ("kibi",prefixValue kibi ),
>   ("mebi",prefixValue mebi ),
>   ("gibi",prefixValue gibi ),
>   ("tebi",prefixValue tebi ),
>   ("pebi",prefixValue pebi ),
>   ("exbi",prefixValue exbi ),
>   ("zebi",prefixValue zebi ),
>   ("yobi",prefixValue yobi )
>   ]

>-- | https://en.wikipedia.org/wiki/Binary_prefix#kibi

>kibi,mebi,gibi,tebi,pebi,exbi,zebi,yobi :: (VectorSpace u, Num (Scalar u)) => u -> u
>kibi = (1024 =*)
>mebi = (1048576 =*)
>gibi = (1073741824 =*)
>tebi = (1099511627776 =*)
>pebi = (1125899906842624 =*)
>exbi = (1152921504606846976 =*)
>zebi = (1180591620717411303424 =*)
>yobi = (1208925819614629174706176 =*)

>deci,centi,milli,micro,nano,pico,femto,atto,zepto,yocto,ronto,quecto :: (VectorSpace u, Floating (Scalar u)) => u -> u
>deci x = 0.1 =* x
>centi x = 0.01 =* x
>milli x = 0.001 =* x
>micro x = (10**(-6)) =* x
>nano  x = (10**(-9)) =* x
>pico  x = (10**(-12)) =* x
>femto x = (10**(-15)) =* x
>atto  x = (10**(-18)) =* x
>zepto x = (10**(-21)) =* x
>yocto x = (10**(-24)) =* x
>ronto x = (10**(-27)) =* x
>quecto x = (10**(-30)) =* x

>(=+=) :: (VectorSpace v) => v -> v -> v
>(=+=) = (%+)

>(=-=) :: (VectorSpace v) => v -> v -> v
>x =-= y = x %+ vnegate y

>instance Foldable Quantity where
>   foldMap f (x `As` d) = f x

>instance Traversable Quantity where
>   traverse f (fa `As` d) = (`As` d) <$> f fa

>instance (Show r, Limiting Stream r) => Limiting Stream (Quantity r) where
>   data Closure Stream (Quantity r) = QuantityClosure (Closure Stream r) Dimension
>   limit z@(Pre (x `As` d) r@(Pre (y `As` d') _))
>     | d == d' = let QuantityClosure yr _ = limit r in QuantityClosure (limit (Pre x $ approximations yr)) d
>     | otherwise = invalidDimensions "limit" d d' x y
>   approximations (QuantityClosure a d) = fmap (`As` d) (approximations a)

>instance (ShowPrecision r, Show (Closure Stream r), Floating r, Ord r) => Show (Stream.Closure Stream (Quantity r)) where
>   show (QuantityClosure s d) = show s ++ (if isDimensionless d then "" else ":") ++ show d

>instance (Show r,Fractional r) => Fractional (Quantity r) where
>   (x `As` r) / (y `As` r') = (x/y) `As` (r %- r')
>   recip (x `As` r) = (recip x) `As` (vnegate r)
>   fromRational x = fromRational x `As` vzero

>instance (Show (Closure Stream r), Fractional (Closure Stream r)) => Fractional (Closure Stream (Quantity r)) where
>   (QuantityClosure x d) / (QuantityClosure y d') = QuantityClosure (x/y) (d %- d')
>   recip (QuantityClosure x d) = QuantityClosure (recip x) (vnegate d)
>   fromRational x = QuantityClosure (fromRational x) dimensionless

>requireDimensionless :: (Show a) => String -> (a -> a) -> Quantity a -> Quantity a
>requireDimensionless op f ~(x `As` r)
>   | isDimensionless r = f x `As` dimensionless
>   | otherwise = invalidDimensions op r dimensionless x (f x)

>instance (Show (Closure Stream a), RealFloat (Closure Stream a)) => RealFloat (Closure Stream (Quantity a)) where
>   floatRadix (QuantityClosure x _) = floatRadix x
>   floatDigits (QuantityClosure x _) = floatDigits x
>   floatRange (QuantityClosure x _) = floatRange x
>   decodeFloat (QuantityClosure x _) = decodeFloat x
>   encodeFloat a b = QuantityClosure (encodeFloat a b) dimensionless
>   exponent (QuantityClosure x _) = exponent x
>   significand (QuantityClosure x d) = QuantityClosure (significand x) d
>   scaleFloat a (QuantityClosure x d) = QuantityClosure (scaleFloat a x) d
>   isNaN (QuantityClosure x _) = isNaN x
>   isInfinite (QuantityClosure x _) = isInfinite x
>   isDenormalized (QuantityClosure x _) = isDenormalized x
>   isNegativeZero (QuantityClosure x _) = isNegativeZero x
>   isIEEE (QuantityClosure x _) = isIEEE x
>   atan2 (QuantityClosure x d) (QuantityClosure y d')
>     | d == d' = QuantityClosure (atan2 x y) d

>instance (Show a, RealFloat a) => RealFloat (Quantity a) where
>   floatRadix (x `As` _) = floatRadix x
>   floatDigits (x `As` _) = floatDigits x
>   floatRange (x `As` _) = floatRange x
>   decodeFloat (x `As` _) = decodeFloat x
>   encodeFloat i j = encodeFloat i j `As` dimensionless
>   exponent (x `As` _) = exponent x
>   significand (x `As` d) = significand x `As` d
>   scaleFloat i (x `As` d) = scaleFloat i x `As` d
>   isNaN (x `As` d) = isNaN x
>   isInfinite (x `As` d) = isInfinite x
>   isNegativeZero (x `As` d) = isNegativeZero x
>   isDenormalized (x `As` d) = isDenormalized x
>   isIEEE (x `As` d) = isIEEE x
>   atan2 (x `As` d) (y `As` d')
>     | d == d' = atan2 x y `As` radianDimension
>     | otherwise = invalidDimensions "atan2" d d' x y

>instance (Show r, ConjugateSymmetric r) => ConjugateSymmetric (Quantity r) where
>   conj (x `As` d) = (conj x) `As` d

>liftQuantityClosure :: (Closure Stream a -> Closure Stream b) -> Closure Stream (Quantity a) -> Closure Stream (Quantity b)
>liftQuantityClosure f (QuantityClosure x d) = QuantityClosure (f x) d

>liftQuantityClosure2 :: (Show (Closure Stream a), Show (Closure Stream b))
>   => String
>   -> (Closure Stream a -> Closure Stream b -> Closure Stream c)
>   -> Closure Stream (Quantity a) -> Closure Stream (Quantity b) -> Closure Stream (Quantity c)
>liftQuantityClosure2 msg f (QuantityClosure x d) (QuantityClosure y d')
>    | d == d' = QuantityClosure (f x y) d
>    | otherwise = invalidDimensions msg d d' x y

>instance (Show (Closure Stream r), Floating (Closure Stream r)) => Floating (Closure Stream (Quantity r)) where
>  pi = QuantityClosure pi dimensionless
>  exp = liftQuantityClosure exp
>  log = liftQuantityClosure log
>  (**) = liftQuantityClosure2 "(**)-closure" (**)
>  logBase = liftQuantityClosure2 "logBase-closure" logBase
>  sqrt = liftQuantityClosure sqrt
>  sin = liftQuantityClosure sin
>  cos = liftQuantityClosure cos
>  tan = liftQuantityClosure tan
>  asin = liftQuantityClosure asin
>  acos = liftQuantityClosure acos
>  atan = liftQuantityClosure atan
>  sinh = liftQuantityClosure sinh
>  cosh = liftQuantityClosure cosh
>  tanh = liftQuantityClosure tanh
>  asinh = liftQuantityClosure asinh
>  acosh = liftQuantityClosure acosh
>  atanh = liftQuantityClosure atanh

>piComplexQuantity :: (RealFloat a) => Quantity (Complex a)
>piComplexQuantity = pi `As` radianDimension

>sqrtComplexQuantity :: (RealFloat a) => Quantity (Complex a) -> Quantity (Complex a)
>sqrtComplexQuantity (c `As` q) = sqrt c `As` (q/2)

>powerComplexQuantity :: (RealFloat a) => Quantity (Complex a) -> Rational -> Quantity (Complex a)
>powerComplexQuantity (x `As` r) q = (x ** fromRational q) `As` (q %* r)

>expComplexQuantity :: (RealFloat a, Show a) => Quantity (Complex a) -> Quantity (Complex a)
>expComplexQuantity = requireDimensionless "exp" exp

>logComplexQuantity :: (RealFloat a, Show a) => Quantity (Complex a) -> Quantity (Complex a)
>logComplexQuantity = requireDimensionless "log" log

>sinComplexQuantity :: (RealFloat a, Show a) => Quantity (Complex a) -> Quantity (Complex a)
>sinComplexQuantity = requireDimensionless "sin" sin
>cosComplexQuantity :: (RealFloat a, Show a) => Quantity (Complex a) -> Quantity (Complex a)
>cosComplexQuantity = requireDimensionless "cos" cos
>tanComplexQuantity :: (RealFloat a, Show a) => Quantity (Complex a) -> Quantity (Complex a)
>tanComplexQuantity = requireDimensionless "tan" tan
>asinComplexQuantity :: (RealFloat a, Show a) => Quantity (Complex a) -> Quantity (Complex a)
>asinComplexQuantity = requireDimensionless "asin" asin
>acosComplexQuantity :: (RealFloat a, Show a) => Quantity (Complex a) -> Quantity (Complex a)
>acosComplexQuantity = requireDimensionless "acos" acos
>atanComplexQuantity :: (RealFloat a, Show a) => Quantity (Complex a) -> Quantity (Complex a)
>atanComplexQuantity = requireDimensionless "atan" atan
>sinhComplexQuantity :: (RealFloat a, Show a) => Quantity (Complex a) -> Quantity (Complex a)
>sinhComplexQuantity = requireDimensionless "sinh" sinh
>coshComplexQuantity :: (RealFloat a, Show a) => Quantity (Complex a) -> Quantity (Complex a)
>coshComplexQuantity = requireDimensionless "cosh" cosh
>tanhComplexQuantity :: (RealFloat a, Show a) => Quantity (Complex a) -> Quantity (Complex a)
>tanhComplexQuantity = requireDimensionless "tanh" tanh
>asinhComplexQuantity :: (RealFloat a, Show a) => Quantity (Complex a) -> Quantity (Complex a)
>asinhComplexQuantity = requireDimensionless "asinh" asinh
>acoshComplexQuantity :: (RealFloat a, Show a) => Quantity (Complex a) -> Quantity (Complex a)
>acoshComplexQuantity = requireDimensionless "acosh" acosh
>atanhComplexQuantity :: (RealFloat a, Show a) => Quantity (Complex a) -> Quantity (Complex a)
>atanhComplexQuantity = requireDimensionless "atanh" atanh

>instance (RationalRoots a) => RationalRoots (Quantity a) where
>   rationalPower (x `As` r) q = rationalPower x q `As` (q %* r)

>instance (Real a, Floating a, Show a) => Floating (Quantity a) where
>   pi = pi `As` radianDimension
>   sqrt (x `As` r) = sqrt x `As` (r / 2)
>   (x `As` r) ** (y `As` q)
>     | isDimensionless q = (x ** y) `As` (toRational y %* r)
>     | otherwise = invalidDimensions "**" q dimensionless y y
>   exp = requireDimensionless "exp" exp
>   log = requireDimensionless "log" log
>   sin = requireDimensionless "sin" sin
>   cos = requireDimensionless "cos" cos
>   tan = requireDimensionless "tan" tan
>   asin = requireDimensionless "asin" asin
>   acos = requireDimensionless "acos" acos
>   atan = requireDimensionless "atan" atan
>   sinh = requireDimensionless "sinh" sinh
>   cosh = requireDimensionless "cosh" cosh
>   tanh = requireDimensionless "tanh" tanh
>   asinh = requireDimensionless "asinh" asinh
>   acosh = requireDimensionless "acosh" acosh
>   atanh = requireDimensionless "atanh" atanh


>instance (Ord r, Num r, Show r) => Ord (Quantity r) where
>   compare (x `As` r) (y `As` r')
>     | r == r' = compare (x-y) 0
>     | otherwise = invalidDimensions "compare" r r' x y

>instance (Show (Closure Stream r), Num (Closure Stream r), Ord (Closure Stream r)) => Ord (Closure Stream (Quantity r)) where
>   compare (QuantityClosure x d) (QuantityClosure x' d')
>     | d == d' = compare (x-x') 0
>     | otherwise = invalidDimensions "compare-closure" d d' x x'

>instance (Show r,RealFrac r) => RealFrac (Quantity r) where
>   properFraction (x `As` r) = let (b,c) = properFraction x in (b,c `As` r)
>   round (x `As` r) = round x
>   truncate (x `As` r) = truncate x
>   ceiling (x `As` r) = ceiling x
>   floor (x `As` r) = floor x

>instance (Show (Closure Stream r), RealFrac (Closure Stream r)) => RealFrac (Closure Stream (Quantity r)) where
>   properFraction (QuantityClosure x d) = let (b,c) = properFraction x in (b,QuantityClosure c d)
>   round (QuantityClosure x _) = round x
>   ceiling (QuantityClosure x _) = ceiling x
>   truncate (QuantityClosure x _) = truncate x
>   floor (QuantityClosure x _) = floor x

>instance (Show r, Real r) => Real (Quantity r) where
>   toRational (x `As` r) = toRational x

>instance (Show (Closure Stream r), Real (Closure Stream r)) => Real (Closure Stream (Quantity r)) where
>   toRational (QuantityClosure x _) = toRational x

>readprefix :: TRead.ReadPrec (Quantity Double -> Quantity Double)
>readprefix = choose $ map (\(a,b) -> (a,return b)) siPrefixes

>readunit :: TRead.ReadPrec (String,Quantity Double)
>readunit = choose $ map (\ (a,b) -> (a,return (a,b))) siUnits

>deriving instance Read (Quantity (Complex Double))
>deriving instance Read (Quantity (Complex Float))
>deriving instance Read (Quantity Integer)
>deriving instance Read (Quantity Int)

>-- | This read instance handles input examples such
>--   as "10 m/(s*s)", "38.4 F", "12 As", "13 kgm/(s*s)",
>--     "3.4 mm"
>instance Read (Quantity Double) where
>   readPrec = do
>     v <- TRead.readPrec
>     whitespace
>     (do prefix <- readprefix
>         lst <- some readunit
>         lst' <- ((whitespace >> string "/" >> unitdivider) >>= (return . Just)) TRead.<++ return Nothing
>         let lst'' = maybe lst (\lst2 -> lst ++ map (\(i,j) -> (i,1 / j)) lst2) lst'
>         return $! prefix $ v %* (product $ map snd lst''))
>       TRead.<++ do
>             lst <- many readunit
>             lst' <- ((whitespace >> string "/" >> whitespace >> unitdivider) >>= (return . Just)) <|> return Nothing
>             let lst'' = maybe lst (\lst2 -> lst ++ map (\(i,j) -> (i,1 / j)) lst2) lst'
>             return $! v %* (product $ map snd lst'')
>      where 
>            parens x = string "(" >> whitespace >> TRead.reset x >>= \v -> whitespace >> string ")" >> return v
>            chooseunits = TRead.step (readunit >>= \v -> whitespace >> string "*" >> whitespace >> chooseunits >>= \r -> return (v:r))
>                        TRead.<++ (readunit >>= (\x -> return [x]))
>            unitdivider = (readunit >>= (\x -> whitespace >> string "/" >> unitdivider >>= \xr -> (return (x:xr))))
>                           TRead.<++ (readunit >>= (\x -> return [x]))
>                           TRead.<++ parens chooseunits
  
>instance (ShowPrecision r) => Show (Quantity r) where
>  show (x `As` d) = showAtPrecision x 10 ++ " " ++ show d

>instance (ShowPrecision r) => PpShow (Quantity r) where
>  pp (x `As` d) = pp (showAtPrecision x 10) Pretty.<+> pp d


>invalidDimensions :: (Show b, Show c) => String -> Dimension -> Dimension -> b -> c -> a
>invalidDimensions op r r' a b= throw $ InvalidDimensionsException r r' $
>     op ++ ":Invalid dimensions: " ++ show r ++ " != " ++ show r' ++ "; " ++ show a ++ "<>" ++ show b
>invalidDimensionsM :: (MonadFail m, Show b, Show c) => String -> Dimension -> Dimension -> b -> c -> m a
>invalidDimensionsM op r r' a b = fail $
>     op ++ ":Invalid dimensions: " ++ show r ++ " != " ++ show r' ++ "; " ++ show a ++ "<>" ++ show b

>instance (Enum r, Show r) => Enum (Quantity r) where
>   succ (a `As` r) = succ a `As` r
>   pred (a `As` r) = pred a `As` r
>   toEnum i = toEnum i `As` dimensionless
>   fromEnum (a `As` r)
>     | isDimensionless r = fromEnum a
>     | otherwise = invalidDimensions "fromEnum" r dimensionless a a                 
>   enumFrom (a `As` r) = map (`As` r) $ enumFrom a
>   enumFromThen (a `As` r) (b `As` r')
>     | r == r' = map (`As` r) $ enumFromThen a b
>     | otherwise = invalidDimensions "enumFromThen" r r' a b
>   enumFromTo (a `As` r) (b `As` r')
>     | r == r' = map (`As` r) $ enumFromTo a b
>     | otherwise = invalidDimensions "enumFromTo" r r' a b
>   enumFromThenTo (a `As` r) (b `As` r') (c `As` r'')
>     | r == r' && r == r'' = map (`As` r) $ enumFromThenTo a b c
>     | r == r'   = invalidDimensions "enumFromThenTo" r r'' a c
>     | otherwise = invalidDimensions "enumFromThenTo" r r' a b

>instance (Integral r, Show r) => Integral (Quantity r) where
>   quot (a `As` r) (b `As` r') = (a `quot` b) `As` (r %- r')
>   rem (a `As` r) (b `As` r') = (a `rem` b) `As` (r %- r')
>   div (a `As` r) (b `As` r') = (a `div` b) `As` (r %- r')
>   mod (a `As` r) (b `As` r') = (a `mod` b) `As` (r %- r')
>   quotRem (a `As` r) (b `As` r') = let (a',b') = quotRem a b
>                                        d = r %- r'
>                                    in (a' `As` d, b' `As` d)
>   divMod (a `As` r) (b `As` r') = let (a',b') = divMod a b
>                                       d = r %- r'
>                                    in (a' `As` d, b' `As` d)
>   toInteger (a `As` d)
>     | isDimensionless d = toInteger a
>     | otherwise = invalidDimensions "toInteger" d dimensionless a a

>plusQ :: (MonadFail m, Num r, Show r) => Quantity r -> Quantity r -> m (Quantity r)
>plusQ (x `As` r) (y `As` r')
>   | r /= r' = invalidDimensionsM "plusQ" r r' x y
>   | otherwise = return $! (x+y) `As` r
>
>minusQ :: (MonadFail m, Num r, Show r) => Quantity r -> Quantity r -> m (Quantity r)
>minusQ (x `As` r) (y `As` r')
>   | r /= r' = invalidDimensionsM "minusQ" r r' x y
>   | otherwise = return $! (x - y) `As` r

>instance (Num r, Show r) => Num (Quantity r) where
>  (x `As` r) + (y `As` r')
>      | r /= r' = invalidDimensions "+" r r' x y
>      | otherwise = (x+y) `As` r
>  (x `As` r) - (y `As` r')
>      | r /= r' = invalidDimensions "-" r r' x y
>      | otherwise = (x - y) `As` r
>  (x `As` r) * (y `As` r') = (x*y) `As` (r %+ r')
>  negate (x `As` r) = (negate x) `As` r
>  abs (x `As` r) = (abs x) `As` r
>  signum (x `As` r) = (signum x) `As` dimensionless
>  fromInteger a = (fromInteger a) `As` dimensionless

>instance (Show (Closure Stream r), Num (Closure Stream r)) => Num (Closure Stream (Quantity r)) where
>  (+) = liftQuantityClosure2 "(+)-closure" (+)
>  (-) = liftQuantityClosure2 "(-)-closure" (-)
>  (QuantityClosure x d) * (QuantityClosure x' d') = QuantityClosure (x*x') (d %+ d')
>  negate = liftQuantityClosure negate
>  abs = liftQuantityClosure abs
>  signum = liftQuantityClosure signum
>  fromInteger a = QuantityClosure (fromInteger a) dimensionless


>instance (Num r) => VectorSpace (Quantity r) where
>   type Scalar (Quantity r) = r
>   vzero = 0 `As` dimensionless
>   vnegate (x `As` d) = (negate x) `As` d
>   (x `As` d) %+ (y `As` d')
>      | d == d' = (x + y) `As` d
>      | otherwise = invalidDimensions "vector sum" d d' "" "" -- don't want Show req.
>   a %* (x `As` d) = (a * x) `As` d

>instance (Num r, NormedSpace r) => NormedSpace (Quantity r) where
>   norm (x `As` _) = x



>showDimension :: Dimension -> String
>showDimension (Dimension a b c d e f g) = if str /= "" then take (length str - 1) str else ""
> where str = showz "A" d ++ showz "kg" b ++ showz "m" a ++  showz "s" c ++ showz "K" e ++ showz "cd" f ++ showz "mol" g
>       showz u 0 = ""
>       showz u 1 = u ++ " "
>       showz u i
>           | denominator i == 1 = u ++ "^" ++ show (numerator i) ++ " "
>           | otherwise = u ++ "^(" ++ show i ++ ") "

>fullDimensionTable :: [(Dimension,String)]
>fullDimensionTable = res4
>  where res4 = dimensionTable `interleave` map mapper dimensionTable
>        mapper (dim,p) = (vnegate dim, p ++ "^-1")

order in the table is significant

>dimensionTable :: [(Dimension,String)]
>dimensionTable = [
>  (Dimension (-2) (-1) 4 2 0 0 0 , "F"),
>  (Dimension 2 1 (-3) (-2) 0 0 0 , "Ω"),
>  (Dimension (-2) (-1) 3 2 0 0 0 , "S"),
>  (Dimension 2 1 (-3) (-1) 0 0 0 , "V"),
>  (Dimension 2 1 (-3) 0 0 0 0 , "W"),
>  (Dimension (-1) 1 (-2) 0 0 0 0 , "Pa"),
>  (Dimension 2 1 (-2) (-2) 0 0 0 , "H"),
>  (Dimension 2 1 (-2) (-1) 0 0 0 , "Wb"),
>  (Dimension 2 1 (-2)  0 0 0 0 , "J"),
>  (Dimension 1 1 (-2) 0 0 0 0 , "N"),
>  (Dimension 0 1 (-2) (-1) 0 0 0 , "T"),
>  (Dimension 0 0 1 1 0 0 0 , "C"),
>  (Dimension 1 0 0 0 0 0 0 ,"m"),
>  (Dimension 0 1 0 0 0 0 0 , "kg"),
>  (Dimension 0 0 1 0 0 0 0 , "s"),
>  (Dimension 0 0 0 1 0 0 0 , "A"),
>  (Dimension 0 0 0 0 1 0 0 , "K"),
>  (Dimension 0 0 0 0 0 1 0 , "cd"),
>  (Dimension 0 0 0 0 0 0 1 , "mol")
>  ]

>instance Read Dimension where
>   readPrec = do lst <- dimensionProduct_reader
>                 lst' <- ((optional_whitespace >> string "/" >> dimension_divider_reader) >>= (return . Just)) TRead.<++ return Nothing
>                 let lst'' = maybe lst (\lst2 -> lst ++ map (\j -> negate j) lst2) lst'
>                 return $! sum lst''

>dimensionProduct_reader :: TRead.ReadPrec [Dimension]
>dimensionProduct_reader = TRead.step $ do
>                                     v <- dimension_reader
>                                     optional_whitespace
>                                     string "*"
>                                     optional_whitespace
>                                     r <- dimensionProduct_reader
>                                     return $! (v:r)
>                            TRead.<++ some dimension_reader
>                            TRead.<++ TRead.parens dimensionProduct_reader

>dimension_divider_reader :: TRead.ReadPrec [Dimension]
>dimension_divider_reader = do
>        x <- dimension_reader
>        whitespace
>        string "/"
>        xr <- dimension_divider_reader
>        return $! (x:xr)
>  TRead.<++ (dimension_reader >>= \x -> return $! [x])
>  TRead.<++ TRead.parens dimensionProduct_reader

>dimension_reader :: TRead.ReadPrec Dimension
>dimension_reader = choose $ fmap (\ (d,s) -> (s,return d)) fullDimensionTable

>dimensionless :: Dimension
>dimensionless = Dimension 0 0 0 0 0 0 0 


>instance PpShow Dimension where
>   pp z@(Dimension a b c d e f g) = maybe (def z) id $ foldl check Nothing fullDimensionTable
>     where check Nothing (z'@(Dimension a' b' c' d' e' f' g'),p)
>                    | (abs a >= abs a') && (signum a == signum a' || signum a' == 0)
>                      && (abs b >= abs b') && (signum b == signum b'|| signum b' == 0)
>                      && (abs c >= abs c') && (signum c == signum c' || signum c' == 0)
>                      && (abs d >= abs d') && (signum d == signum d' || signum d' == 0)
>                      && (abs e >= abs e') && (signum e == signum e' || signum e' == 0)
>                      && (abs f >= abs f') && (signum f == signum f' || signum f' == 0)
>                      && (abs g >= abs g') && (signum g == signum g' || signum g' == 0)
>                            = Just $ pp p Pretty.<+> pp (z %- z')
>                    | otherwise = Nothing
>           check (Just x) _ = Just x                      
>           def (Dimension 0 0 0 0 0 0 0) = Pretty.empty
>           def z = ppDimension z

>-- | ppDimension prints dimension in a simple way without interpreting derived dimensions
>-- for example, @ppDimension newtonDimension == pp "kg m s^-2"@
>ppDimension :: Dimension -> Pretty.Doc
>ppDimension (Dimension a b c d e f g) = def b "kg" Pretty.<+> def a "m"
>                                           Pretty.<+> def c "s"
>                                           Pretty.<+> def d "A"
>                                           Pretty.<+> def e "K"
>                                           Pretty.<+> def f "cd"
>                                           Pretty.<+> def g "mol"
>  where def x str | x == 1 = pp str
>                  | x == 0 = Pretty.empty
>                  | otherwise = pp str <> pp '^' <> pp_ratio x
>        pp_ratio z
>           | denominator z == 1 = pp (numerator z)
>           | otherwise = pp '{' <> pp z <> pp '}'

>instance Show Dimension where
>  show z@(Dimension a b c d e f g) = maybe (def z) id $ foldl check Nothing fullDimensionTable
>    where check Nothing (z'@(Dimension a' b' c' d' e' f' g'),p) 
>                    | (abs a >= abs a') && (signum a == signum a' || signum a' == 0)
>                      && (abs b >= abs b') && (signum b == signum b'|| signum b' == 0)
>                      && (abs c >= abs c') && (signum c == signum c' || signum c' == 0)
>                      && (abs d >= abs d') && (signum d == signum d' || signum d' == 0)
>                      && (abs e >= abs e') && (signum e == signum e' || signum e' == 0)
>                      && (abs f >= abs f') && (signum f == signum f' || signum f' == 0)
>                      && (abs g >= abs g') && (signum g == signum g' || signum g' == 0)
>                            = let rest = z %- z'
>                               in if isDimensionless rest
>                                    then Just p
>                                    else Just (p ++ " " ++ show rest)
>                    | otherwise = Nothing
>          check (Just x) _ = Just x                      
>          def (Dimension 0 0 0 0 0 0 0) = ""
>          def z = showDimension z

>kelvinDimension :: Dimension
>kelvinDimension = Dimension 0 0 0 0 1 0 0 

>faradDimension :: Dimension
>faradDimension  = Dimension (-2) (-1) 4 2 0 0 0 
>ohmDimension :: Dimension
>ohmDimension    = Dimension 2 1 (-3) (-2) 0 0 0 
>sievertDimension :: Dimension
>sievertDimension = Dimension (-2) (-1) 3 2 0 0 0 
>voltDimension :: Dimension
>voltDimension    = Dimension 2 1 (-3) (-1) 0 0 0 

>pascalDimension :: Dimension
>pascalDimension  = Dimension (-1) 1 (-2) 0 0 0 0 

>henryDimension :: Dimension
>henryDimension   = Dimension 2 1 (-2) (-2) 0 0 0 

>weberDimension :: Dimension
>weberDimension   = Dimension 2 1 (-2) (-1) 0 0 0 
> 
>jouleDimension :: Dimension
>jouleDimension   = Dimension 2 1 (-2) 0 0 0 0 
> 
>newtonDimension :: Dimension
>newtonDimension  = Dimension 1 1 (-2) 0 0 0 0 
> 
>teslaDimension :: Dimension
>teslaDimension   = Dimension 0 1 (-2) (-1) 0 0 0 
> 
>meterDimension :: Dimension
>meterDimension    = Dimension 1 0 0 0 0 0 0 
> 
>kilogramDimension :: Dimension
>kilogramDimension = Dimension 0 1 0 0 0 0 0 
> 
>secondDimension :: Dimension
>secondDimension   = Dimension 0 0 1 0 0 0 0 
> 
>ampereDimension :: Dimension
>ampereDimension   = Dimension 0 0 0 1 0 0 0 
> 
>candelaDimension :: Dimension
>candelaDimension  = Dimension 0 0 0 0 0 1 0 
> 
>molDimension :: Dimension
>molDimension = Dimension 0 0 0 0 0 0 1  

>-- | radianDimension is basically same as dimensionless, using separate name anyway
>-- to allow clients to distinguish. No checking for this distinction is implemented.
>radianDimension :: Dimension
>radianDimension = meterDimension %- meterDimension
>
>-- | steradianDimension is basically same as dimensionless, redefining anyway
>-- to allow clients to distinguish steradians. No checking for this distinction is implemented.
>steradianDimension :: Dimension
>steradianDimension = (2 %* meterDimension) %- (2 %* meterDimension)

>steradian :: (Floating a) => Quantity a
>steradian = 1 @@ steradianDimension
>becquerelDimension :: Dimension
>becquerelDimension = 0 %- secondDimension
>becquerel :: (Floating a) => Quantity a
>becquerel = 1 @@ becquerelDimension
>grayDimension :: Dimension
>grayDimension = (2 %* meterDimension) %- (2 %* secondDimension)
>gray :: (Floating a) => Quantity a
>gray = 1 @@ grayDimension
>luxDimension = candelaDimension %- (2 %* meterDimension)
>lux :: (Floating a) => Quantity a
>lux = 1 @@ luxDimension

>lumenDimension :: Dimension
>lumenDimension = candelaDimension %+ steradianDimension
>lumen :: (Floating a) => Quantity a
>lumen = 1 @@ lumenDimension
>kelvinQuantity :: (Floating a) => Quantity a
>kelvinQuantity = 1 @@ kelvinDimension

>siemens :: (Floating a) => Quantity a
>siemens = 1 @@ siemensDimension

>second :: (Floating a) => Quantity a
>second = 1.0 @@ secondDimension

>meter :: (Floating a) => Quantity a
>meter = 1.0 @@ meterDimension
>
>kilogram :: (Floating a) => Quantity a
>kilogram = 1.0 @@ kilogramDimension
>
>gram :: (Floating a) => Quantity a
>gram = milli kilogram
> 
>mole :: (Floating a) => Quantity a
>mole = 1.0 @@ molDimension
>candela :: (Floating a) => Quantity a 
>candela = 1.0 @@ candelaDimension
>kelvin :: (Floating a) => Quantity a
>kelvin = 1.0 @@ kelvinDimension
>ampere :: (Floating a) => Quantity a
>ampere = 1.0 @@ ampereDimension
>coulomb :: (Floating a) => Quantity a
>coulomb = 1.0 @@ coulombDimension
>tesla :: (Floating a) => Quantity a
>tesla = 1.0 @@ teslaDimension

>henry :: (Floating a) => Quantity a
>henry = 1.0 @@ henryDimension

>newton :: (Floating a) => Quantity a
>newton = 1.0 @@ newtonDimension

>pascal :: (Floating a) => Quantity a
>pascal = 1.0 @@ pascalDimension

>joule :: (Floating a) => Quantity a
>joule = 1.0 @@ jouleDimension

>weber :: (Floating a) => Quantity a
>weber = 1.0 @@ weberDimension

>volt :: (Floating a) => Quantity a
>volt = 1.0 @@ voltDimension

>sievert :: (Floating a) => Quantity a
>sievert = 1.0 @@ sievertDimension

>ohm :: (Floating a) => Quantity a
>ohm = 1.0 @@ ohmDimension

>farad :: (Floating a) => Quantity a
>farad = 1.0 @@ faradDimension

>watt :: (Floating a) => Quantity a
>watt = 1.0 @@ wattDimension

>minute :: (Floating a) => Quantity a
>minute = 60 %* second

>hour :: (Floating a) => Quantity a
>hour = 60 %* minute

>day :: (Floating a) => Quantity a
>day = 24 %* hour

>week :: (Floating a) => Quantity a
>week = 7 %* day

>year :: (Floating a) => Quantity a
>year = 365 %* day

>-- | one degree angle
>degree :: (Floating a) => Quantity a
>degree = fromDegreesAngle 1.0

>radian :: (Floating a) => Quantity a
>radian = 1.0 @@ radianDimension
>katalDimension :: Dimension
>katalDimension = molDimension %- secondDimension
>katal :: (Floating a) => Quantity a
>katal = 1.0 @@ katalDimension

>-- | conversion from degrees celcius.
>-- WARNING: produces degrees in kelvin, so computations intended
>-- in degrees celcius should be done before conversion!
>fromCelsius :: (Show a, Fractional a) => a -> Quantity a
>fromCelsius x = x @@ kelvinDimension + (273.15 @@ kelvinDimension)

>fromFahrenheit :: (Fractional a, Show a) => a -> Quantity a
>fromFahrenheit x = ((x + 459.67) * (5/9)) @@ kelvinDimension

>fromRankine :: (Fractional a, Show a) => a -> Quantity a
>fromRankine x = (x*(5/9)) @@ kelvinDimension

>toFahrenheit :: (MonadFail m, Fractional a,VectorSpace a)
> => Quantity a -> m a
>toFahrenheit x
>   | dimension x == kelvinDimension = return $! (amount x * (9/5)) - 459.67
>   | otherwise = fail "Cannot convert to fahrenheit"

>toCelsius :: (Fractional a, Show a) => Quantity a -> a
>toCelsius x = valueAmount (x - 273.15 @@ kelvinDimension)

>toRankine :: (Fractional a, Show a) => Quantity a -> a
>toRankine x = valueAmount x * 9 / 5

>fromRadiansAngle :: (Floating a) => a -> Quantity a
>fromRadiansAngle r = r `As` radianDimension

>-- | conversion from angle.
>-- WARNING: produces degrees in radian, so computations intended
>-- in degrees should be done before conversion!
>fromDegreesAngle :: (Floating a) => a -> Quantity a
>fromDegreesAngle alfa = (alfa * pi / 180.0) `As` radianDimension

>hertzDimension :: Dimension
>hertzDimension = dimensionless %- secondDimension
>hertz :: (Floating a) => Quantity a
>hertz = 1.0 @@ hertzDimension

>squaremeterDimension :: Dimension
>squaremeterDimension = 2 %* meterDimension

>squaremeter :: (Floating a) => Quantity a
>squaremeter = 1.0 @@ squaremeterDimension
>
>cubicmeterDimension :: Dimension
>cubicmeterDimension = 3 %* meterDimension
>cubicmeter :: (Floating a) => Quantity a
>cubicmeter = 1.0 @@ cubicmeterDimension

>wattDimension :: Dimension
>wattDimension = (kilogramDimension %+ (2 %* meterDimension) %- (3 %* secondDimension))
>coulombDimension :: Dimension
>coulombDimension = (secondDimension %+ ampereDimension)
>siemensDimension :: Dimension
>siemensDimension = (3 %* secondDimension) %+ (2 %* ampereDimension)
>                 %- (kilogramDimension %+ squaremeterDimension)


>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>planck_length :: (Floating a) => Quantity a
>planck_length = 1.61619997e-35 %* meter
>
>planck_mass :: (Floating a) => Quantity a
>planck_mass = 2.1765113e-8 %* kilogram
>planckTime :: (Floating a) => Quantity a
>planckTime = 5.3910632e-44 %* second
>planckCharge :: (Floating a) => Quantity a
>planckCharge = 1.87554595641e-18 %* coulomb
>planckTemperature :: (Floating a) => Quantity a
>planckTemperature = 1.41683385e32 %* kelvin

>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>--  <https://en.wikipedia.org/wiki/2019_redefinition_of_SI_base_units>
>-- Warning: don't confuse with stefan_boltzmannConstant.
>-- Notice value is exact.
>boltzmannConstant :: (Floating a) => Quantity a
>boltzmannConstant = 1.380649e-23 @@ (jouleDimension %- kelvinDimension)

>-- | <https://en.wikipedia.org/wiki/Gravitational_constant>
>gravitationalConstant :: (Floating a) => Quantity a
>gravitationalConstant = 6.6740831e-11 @@ (3 %* meterDimension %+ (negate 1) %* kilogramDimension %+ (negate 2) %* secondDimension)
>
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>standardAccelerationOf_gravity :: (Floating a) => Quantity a
>standardAccelerationOf_gravity = 9.80665 @@ (meterDimension %- 2 %* secondDimension)
> 
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>standardAtmosphere :: (Floating a) => Quantity a
>standardAtmosphere = 101325 %* pascal

>-- | <https://en.wikipedia.org/wiki/Speed_of_light>
>-- Note value is exact.
>speedOf_light :: (Floating a) => Quantity a
>speedOf_light = 299792458.0 @@ (meterDimension %- secondDimension)

>-- | <https://en.wikipedia.org/wiki/Planck_constant>
>-- <https://en.wikipedia.org/wiki/2019_redefinition_of_SI_base_units>
>-- Note value is exact.
>planckConstant :: (Floating a) => Quantity a
>planckConstant = 6.62607015e-34 @@ (jouleDimension %+ secondDimension)

>reducedPlanckConstant :: (Floating a) => Quantity a
>reducedPlanckConstant = (1/(2*pi)) %* planckConstant

>-- | <https://en.wikipedia.org/wiki/Electronvolt>
>electronvolt :: (Floating a) => Quantity a
>electronvolt = 1.6021766208e-19 @@ jouleDimension

>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>magneticConstant :: (Floating a) => Quantity a
>magneticConstant = (4*pi*1e-7) @@ (newtonDimension %- (2 %* ampereDimension))
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>electricConstant :: (Show a, Floating a) => Quantity a
>electricConstant = 1 / (magneticConstant * speedOf_light * speedOf_light)
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>characteristic_impedanceOfVacuum :: (Floating a) => Quantity a
>characteristic_impedanceOfVacuum = 376.730313461 @@ ohmDimension
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>coulombsConstant :: (Floating a) => Quantity a
>coulombsConstant = 8.9875517873681764e9 @@ (kilogramDimension %+ (3 %* meterDimension) %- (4 %* secondDimension) %- (2 %* ampereDimension))
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>--   <https://en.wikipedia.org/wiki/2019_redefinition_of_SI_base_units>
>--   Notice value is exact.
>elementaryCharge :: (Floating a) => Quantity a
>elementaryCharge = 1.602176634e-19 @@ coulombDimension

>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>bohr_magneton :: (Floating a) => Quantity a
>bohr_magneton = 9.27400999457e-24 @@ (jouleDimension %- teslaDimension)
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>conductanceQuantum :: (Floating a) => Quantity a
>conductanceQuantum = 7.748091731018e-5 @@ sievertDimension
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>inverseConductanceQuantum :: (Floating a) => Quantity a
>inverseConductanceQuantum = 12906.403727829 @@ ohmDimension

>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>josephsonConstant :: (Floating a) => Quantity a
>josephsonConstant = 4.83597852530e14 @@ (hertzDimension %- voltDimension)
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>magneticFluxQuantum :: (Floating a) => Quantity a
>magneticFluxQuantum = 2.06783383113e-15 @@ weberDimension
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>nuclear_magneton :: (Floating a) => Quantity a
>nuclear_magneton = 5.05078369931e-27 @@ (jouleDimension %- teslaDimension)
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>von_klitzingConstant :: (Floating a) => Quantity a
>von_klitzingConstant = 25812.807455559 @@ ohmDimension
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>bohr_radius :: (Floating a) => Quantity a
>bohr_radius = 5.291722106712e-11 @@ meterDimension
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>classical_electron_radius :: (Floating a) => Quantity a
>classical_electron_radius = 2.817940322719e-15 @@ meterDimension
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>electron_mass :: (Floating a) => Quantity a
>electron_mass = 9.1093835611e-31 @@ kilogramDimension
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>fermiCouplingConstant :: (Show a,Floating a) => Quantity a
>fermiCouplingConstant = 1.16637876e-5 %* giga (1 / (electronvolt * electronvolt))
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>fineStructureConstant :: (Floating a) => Quantity a
>fineStructureConstant = 7.297352566417e-3 @@ dimensionless

>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>hartree_energy :: (Floating a) => Quantity a
>hartree_energy = 4.35974465054e-18 @@ jouleDimension

>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>proton_mass :: (Floating a) => Quantity a
>proton_mass = 1.67262189821e-27 @@ kilogramDimension
>
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>quantumOfCirculation :: (Floating a) => Quantity a
>quantumOfCirculation = 3.636947548617e-4 @@ (kilogramDimension %+ kilogramDimension %- secondDimension)
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>rydbergConstant :: (Floating a) => Quantity a
>rydbergConstant = 10973731.56850865 @@ ((negate 1) %* meterDimension)
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>thomsonCrossSection :: (Floating a) => Quantity a
>thomsonCrossSection = 6.652458715891e-29 @@ (2 %* meterDimension)

>-- <https://en.wikipedia.org/wiki/2019_redefinition_of_SI_base_units>
>groundStateCesiumHyperfineTransitionFrequency :: (Floating a) => Quantity a
>groundStateCesiumHyperfineTransitionFrequency = 9192631770 @@ hertzDimension

>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>weak_mixingAngle :: (Floating a) => Quantity a
>weak_mixingAngle = 0.222321 @@ radianDimension

>-- | <https://en.wikipedia.org/wiki/2019_redefinition_of_SI_base_units>
>-- value is exact.
>luminousEfficacyOfMonochromatic540e12HertzRadiation :: (Floating a) => Quantity a
>luminousEfficacyOfMonochromatic540e12HertzRadiation = 683 @@ (lumenDimension %- wattDimension)

>-- | <https://en.wikipedia.org/wiki/Avogadro_constant>
>-- <https://en.wikipedia.org/wiki/2019_redefinition_of_SI_base_units>
>-- Notice value is exact.
>avogadroConstant :: (Floating a) => Quantity a
>avogadroConstant = 6.02214076e23 @@ (negate $ molDimension)
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>atomicMassConstant :: (Floating a) => Quantity a
>atomicMassConstant = 1.66053904020e-27 %* kilogram

>faradayConstant :: (Floating a, Show a) => Quantity a
>faradayConstant = 96485.3328959 %* (coulomb / mole)

>firstRadiationConstant :: (Floating a, Show a) => Quantity a
>firstRadiationConstant = 1.19104295315e-16 %* (watt * meter * meter)
>loschmidtConstant :: (Floating a) => Quantity a
>loschmidtConstant = 2.686781115e25 @@ ((-3) %* meterDimension)
>gasConstant :: (Floating a, Show a) => Quantity a
>gasConstant = 8.314459848 %* (joule / (mole * kelvin))
>molarPlanckConstant :: (Floating a, Show a) => Quantity a
>molarPlanckConstant = 3.990312711018e-10 %* (joule * second / mole)
>secondRadiationConstant :: (Floating a, Show a) => Quantity a
>secondRadiationConstant = 1.4387773683e-2 %* (meter * kelvin)

>-- | Warning: don't confuse with boltzmann_constant.
>stefanBoltzmannConstant :: (Floating a, Show a) => Quantity a
>stefanBoltzmannConstant = 5.67036713e-8 %* (watt / (meter^2 * kelvin^4))

>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>efimovFactor :: (Floating a) => Quantity a 
>efimovFactor = 22.7 @@ dimensionless
>
>-- | <https://en.wikipedia.org/wiki/Light-year>
>lightyear :: (Num a) => Quantity a
>lightyear = 9460730472580800 @@ meterDimension

>-- | <https://en.wikipedia.org/wiki/Astronomical_unit>
>astronomicalUnit :: (Floating a) => Quantity a
>astronomicalUnit = 149597870700 %* meter
>-- | <https://en.wikipedia.org/wiki/Parsec>
>parsec :: (Floating a) => Quantity a 
>parsec = (648000 / pi) %* astronomicalUnit

>-- | <https://en.wikipedia.org/wiki/Litre>
>liter = (1 / 1000) %* (meter * meter * meter)
>-- | <https://en.wikipedia.org/wiki/Litre>
>litre = liter

>siDimensions :: [(String, Dimension)]
>siDimensions = [
>  ("eV", jouleDimension),
>  ("m", meterDimension),
>  ("rad", radianDimension),
>  ("sr", steradianDimension),
>  ("K", kelvinDimension),
>  ("s", secondDimension),
>  ("kg", kilogramDimension),
>  ("g", kilogramDimension),
>  ("mol", molDimension),
>  ("cd", candelaDimension),
>  ("A", ampereDimension),
>  ("C", coulombDimension),
>  ("T", teslaDimension),
>  ("H", henryDimension),
>  ("N", newtonDimension),
>  ("Pa", pascalDimension),
>  ("J", jouleDimension),
>  ("Wb", weberDimension),
>  ("V", voltDimension),
>  ("Sv", sievertDimension),
>  ("S", siemensDimension),
>  ("Ω", ohmDimension),
>  ("ohm", ohmDimension),
>  ("F", faradDimension),
>  ("W", wattDimension),
>  ("°C", kelvinDimension),
>  ("°F", kelvinDimension),
>  ("°", radianDimension),
>  ("lm", lumenDimension),
>  ("lx", luxDimension),
>  ("Bq", becquerelDimension),
>  ("Gy", grayDimension),
>  ("kat", katalDimension),
>  ("Hz", hertzDimension),
>  ("ly", meterDimension),
>  ("AU", meterDimension),
>  ("pc", meterDimension),
>  ("l", 3 %* meterDimension),
>  ("B", dimensionless),
>  ("b", dimensionless)]

>-- | <https://en.wikipedia.org/wiki/Parsec>
>--   <https://en.wikipedia.org/wiki/International_System_of_Units>
>siUnits :: [(String,Quantity Double)]
>siUnits = [
>   ("eV",electronvolt),
>   ("m",meter),
>   ("rad", radian),
>   ("sr", steradian),
>   ("K",kelvin),
>   ("s",second),
>   ("kg",kilogram),
>   ("g",gram),
>   ("mol",mole),
>   ("cd",candela),
>   ("A", ampere),
>   ("C", coulomb),
>   ("T", tesla),
>   ("H", henry),
>   ("N", newton),
>   ("Pa", pascal),
>   ("J", joule),
>   ("Wb",weber),
>   ("V",volt),
>   ("Sv", sievert),
>   ("S", siemens),
>   ("Ω", ohm),
>   ("ohm", ohm),
>   ("F", farad),
>   ("W", watt),
>   ("min", minute),
>   ("h", hour),
>   ("d", day),
>   ("w", week),
>   ("y", year),
>   ("°C", fromCelsius 1),
>   ("°F", fromFahrenheit 1),
>   ("°", degree),
>   ("lm", lumen),
>   ("lx", lux),
>   ("Bq", becquerel),
>   ("Gy", gray),
>   ("kat", katal),
>   ("Hz", hertz),
>   ("ly", lightyear),
>   ("AU", astronomicalUnit),
>   ("pc", parsec),
>   ("l",liter),
>   ("B", byte),
>   ("b", bit)
>  ]

>-- | <https://en.wikipedia.org/wiki/Metric_prefix>
>siPrefixes :: (Floating a) => [(String,Prefix a)]
>siPrefixes = [("R", ronna),
>              ("Q", quetta),
>              ("Y", yotta),
>              ("Z", zetta),
>              ("E", exa),
>              ("P", peta),
>              ("T", tera),
>              ("G", giga),
>              ("M", mega),
>              ("k", kilo),
>              ("h", hecto),
>              ("da",deca),
>              ("d",deci),
>              ("c",centi),
>              ("m", milli),
>              ("n", nano),
>              ("µ", micro),
>              ("u", micro),
>              ("p", pico),
>              ("f", femto),
>              ("a", atto),
>              ("z", zepto),
>              ("y", yocto),
>              ("r", ronto),
>              ("q", quecto),
>              ("Ki", kibi),
>              ("Mi", mebi),
>              ("Gi", gibi),
>              ("Ti", tebi),
>              ("Pi", pebi),
>              ("Ei", exbi),
>              ("Zi", zebi),
>              ("Yi", yobi)
>              ]

>-- | this contains a list of quantities where basis of measurement
>-- is not zero in the dimension because the corresponding SI unit
>-- has different zero
>siZeros :: [(String,Quantity Double)]
>siZeros = [
>    ("°C", fromCelsius 0),
>    ("℃", fromCelsius 0),
>    ("°F", fromFahrenheit 0),
>    ("℉", fromFahrenheit 0)
>   ]

