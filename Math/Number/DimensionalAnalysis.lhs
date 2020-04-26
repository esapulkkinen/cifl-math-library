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
>-- @3 %* meter + 4 %* kilogram == throw (InvalidDimensionsException meter_dimension kilogram_dimension "<...>")@
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
>import Math.Number.Stream (Stream(..), Limiting(..), Closed(..), Infinitesimal(..))
>import qualified Math.Number.Stream as Stream
>import Math.Number.Interface ( ShowPrecision(..))
>import Math.Number.Group
>import Math.Number.Interface
>import safe qualified Text.ParserCombinators.ReadPrec as ReadPrec
>import safe qualified Text.Read as TRead
>import safe qualified Math.Matrix.Covector as Covector

>data Quantity r = As {
>   value_amount :: r,
>   value_dimension :: !Dimension }
>  deriving (Typeable, Data, Generic)

>-- | checked projection
>dimensionless_amount :: (Show r) => Quantity r -> r
>dimensionless_amount (x `As` d) | isDimensionless d = x
>                             | otherwise = invalidDimensions "dimensionless_amount" d dimensionless x x

>instance (Show r, DifferentiallyClosed r, VectorSpace r) => DifferentiallyClosed (Quantity r) where
>  derivate f (x `As` d) = derivate (\x' -> value_amount $ f (x' `As` d)) x
>                     `As` (dimension (f (x `As` d)) - d)
>  integral (a `As` da,b `As` db) f
>     | da == db =
>         integral (a,b) (\x -> value_amount (f (x `As` da)))
>         `As` ((dimension $! f (a `As` da)) %+ da)
>     | otherwise = error $ "integral: invalid dimensions: " ++ show da ++ " != " ++ show db

>instance (Show r, Ord r) => Eq (Quantity r) where
>   (x `As` d) == (y `As` d') | d == d' = x == y
>                       | otherwise = invalidDimensions "==" d d' x y

>equal_up_to :: (Floating a, Show a, Ord a) => a -> Quantity a -> Quantity a -> Bool
>equal_up_to tolerance (x `As` d) (y `As` d')
>   | d == d' = abs (x-y) < tolerance
>   | otherwise = invalidDimensions "equal_up_to" d d' x y

>data Dimension = Dimension {
>   length_power :: {-# UNPACK #-} !Rational,
>   weight_power :: {-# UNPACK #-} !Rational,
>   time_power   :: {-# UNPACK #-} !Rational,
>   current_power :: {-# UNPACK #-} !Rational,
>   temperature_power :: {-# UNPACK #-} !Rational,
>   luminosity_power  :: {-# UNPACK #-} !Rational,
>   substance_power   :: {-# UNPACK #-} !Rational }
>  deriving (Eq, Typeable, Ord, Data, Generic)

>instance StandardBasis Dimension where
>  unit_vectors = [meter_dimension,kilogram_dimension,second_dimension,
>                   ampere_dimension, kelvin_dimension, candela_dimension,
>                   mol_dimension]

>dimension_multiples :: Dimension -> Stream Dimension
>dimension_multiples d = Pre d $ fmap (%+ d) $ dimension_multiples d

>quantity_powers :: (Num a, Show a) => Quantity a -> Stream (Quantity a)
>quantity_powers u = Stream.iterate_stream (* u) u

>instance Applicative Quantity where
>  pure x = x `As` dimensionless
>  (f `As` d) <*> (x `As` d')
>    | d == d' = f x `As` d
>    | otherwise = error $ "invalidDimensions: <*>: " ++ show d ++ " != " ++ show d'

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
>   fmap f (x `As` d) = f x `As` d

>data DimensionException = InvalidDimensionsException Dimension Dimension String
>  deriving (Typeable, Show)

>instance Exception DimensionException

>instance (Show r, Infinitesimal r) => Infinitesimal (Quantity r) where
>   epsilon_closure = limit $ fmap (`As` dimensionless) epsilon_stream

>instance (Show r, InnerProductSpace r, Scalar (Quantity r) ~ Scalar r) => InnerProductSpace (Quantity r) where
>   (x `As` d) %. (y `As` d') = x %. y

>mapQuantity :: (a -> b)
>            -> (Dimension -> Dimension)
>            -> Quantity a -> Quantity b
>mapQuantity f g ~(r `As` d) = f r `As` g d

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
>-- 
>--  The fromAmount method must check that compile-time information about dimensions
>-- is sufficient to determine dimension of the given input
>-- e.g. @(fromAmount 3 :: Mass)@ is ok, but @(fromAmount 3 :: Quantity Double)@ is not.
>class (VectorSpace u) => Unit u where
>   amount :: u -> Scalar u
>   fromAmount :: Scalar u -> u
>   fromQuantity :: (Alternative m, Monad m) => Quantity (Scalar u) -> m u
>   unitOf :: u -> String
>   dimension :: u -> Dimension
>   zeroAmount :: (Scalar u -> u) -> Scalar u
>   conversionFactor :: (Scalar u -> u) -> Scalar u
>   zeroAmount _ = 0
>   conversionFactor _ = 1

>fromQuantityDef :: (Monad m, Alternative m, Show a) => Dimension -> (a -> b) -> Quantity a -> m b
>fromQuantityDef dim ctr (x `As` d) = do { guard (d == dim) ; return $! ctr x }
>                                <|> invalidDimensionsM "fromQuantityDef" d dim x x


>instance (Closed a, Show a) => Closed (Quantity a) where
>   accumulation_point s
>     | (Stream.Pre (As x d) xr) <- approximations s
>     = accumulation_point (limit $ Stream.Pre x (fmap value_amount xr)) `As` d

>-- | <https://en.wikipedia.org/wiki/Level_(logarithmic_quantity)
>-- Level represents a reference to which computations involving
>-- logarithmic scales are compared to.
>data Level r = Level {
>   reference_value :: Quantity r,
>   base_of_logarithm :: r }

>deriving instance Read (Level Double)

>scale :: (Show a, Floating a, Real a) => Quantity a -> Level a -> Quantity a
>scale x (Level q b) = log (x / q) / log (pure b)

>logarithmic x (Level q b) = (b ** x) %* q

>hyperbolic x (Level q b) = (b ** ((0 :+ 1) * x)) %* q

>-- | This is a way to convert radians to complex numbers
>-- e.g. @(pi/2) `logarithmic` radian_scale == 0 :+ 1@.
>radian_scale :: (RealFloat a) => Level (Complex a)
>radian_scale = Level (pure (1 :+ 0)) (exp (0 :+ 1))

>bit_scale :: (Num a) => Level a
>bit_scale = Level (1 `As` dimensionless) 2

>bel_scale :: (Num a) => Level a
>bel_scale = Level (1 `As` dimensionless) 10

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
>dB_SPL :: (Floating a) => Level a
>dB_SPL = Level (20 %* micro pascal) 10

>-- | <https://en.wikipedia.org/wiki/Decibel>
>dB_SIL :: (Floating a, Show a) => Level a
>dB_SIL = Level ((10**(negate 12)) %* (watt / squaremeter)) 10

>-- <https://en.wikipedia.org/wiki/Decibel>
>dB_SWL :: (Floating a) => Level a
>dB_SWL = Level (10e-12 %* watt) 10

>-- | for logarithmic lengths in base 10.
>length_scale :: (Floating a) => Level a
>length_scale = Level (1.0 %* meter) 10

>-- | <https://en.wikipedia.org/wiki/Octave>
>-- logarithmic frequency in base 2 relative to 16.352 Hz.
>-- a.k.a. in scientific designation.
>-- middle-C is fourth octave, e.g. @4 \`logarithmic\` octave == 261.626 %* hertz@
>frequency_scale :: (Floating a) => Level a
>frequency_scale = Level (16.352 %* hertz) 2

>-- | <https://en.wikipedia.org/wiki/Octave>
>-- logarithmic frequency in base 2 relative to 16.352 Hz.
>-- a.k.a. in scientific designation.
>-- middle-C is fourth octave, e.g. @4 \`logarithmic\` octave == 261.626 %* hertz@
>-- octave is just a different name for 'frequency_scale'.
>octave :: (Floating a) => Level a
>octave = frequency_scale

>-- | <https://en.wikipedia.org/wiki/Moment_magnitude_scale>
>moment_magnitude_scale :: (Show a, Floating a) => Level a
>moment_magnitude_scale = Level ((10 ** (-7)) %* (newton * meter)) 10

>-- | <https://en.wikipedia.org/wiki/Entropy>
>entropy_scale :: (Floating a) => Level a
>entropy_scale = Level boltzmann_constant (exp 1)

>hartley_scale :: (Num a) => Level a
>hartley_scale = Level (1 `As` dimensionless) 10

>ban_scale :: (Num a) => Level a
>ban_scale = hartley_scale

>time_scale :: (Floating a) => Level a
>time_scale = Level (1 %* second) 10

>weight_scale :: (Floating a) => Level a
>weight_scale = Level (1 %* kilogram) 10

>-- | compute how many nines a probability has. @nines 0.99999 == 5.0@.
>-- <https://en.wikipedia.org/wiki/9#Probability>
>nines :: Double -> Double
>nines prob = value_amount $ negate $ (1.0 `As` dimensionless - prob `As` dimensionless) `scale` bel_scale

>-- | compute probability when given number of nines.
>-- <https://en.wikipedia.org/wiki/9#Probability>
>fromNines :: Double -> Double
>fromNines n = value_amount $ 1.0 - ((negate n) `logarithmic` Level (1.0 `As` dimensionless) 10)

>-- | note frequency from octave and index of note within octave.
>-- @note_pitch 4 0@ is middle-C.
>note_pitch :: Int -> Int -> Quantity Double
>note_pitch oct note_index = (fromIntegral oct + fromIntegral note_index / 12) `logarithmic` frequency_scale

>ratioToDecibel :: (Fractional a, Show a, Floating a, Real a) => Quantity a -> Level a -> Quantity a
>ratioToDecibel x lvl = base_of_logarithm lvl %* (x `scale` lvl)

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
>fromAlternatives count = scale (count `As` dimensionless) bit_scale

>-- | Note that the number of alternatives grows quickly.
>toAlternatives :: (Floating a, Ord a, ShowPrecision a, Monad m) => Quantity a -> m a
>toAlternatives x
>   | isDimensionless (value_dimension x) = return $! 2.0 ** value_amount x
>   | otherwise = invalidDimensionsM "toAlternatives" (value_dimension x) dimensionless x x

>(@@) :: r -> Dimension -> Quantity r
>x @@ y = x `As` y

>convertTo :: (Monad m, Floating a, ShowPrecision a,Ord a, VectorSpace a)
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
>  fromAmount x = error "Ambiguous units while constructing run-time Quantity."

>(=/) :: (Fractional (Scalar a), Unit a, Show a) => a -> a -> Scalar a
>(=/) x y
>   | dimension x == dimension y = amount x / amount y
>   | otherwise = invalidDimensions "(=/)" (dimension x) (dimension y) x y

>-- | conversions between units. Dimensions have to match.
>convert :: (Scalar u ~ Scalar v, Unit v, Unit u, Show v, Show u, Monad m, Fractional (Scalar u))
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

>dimension_basis :: [Dimension]
>dimension_basis = [meter_dimension, kilogram_dimension, second_dimension,
>                   ampere_dimension, kelvin_dimension, candela_dimension,
>                   mol_dimension]

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
>   (Dimension l w t c te lu su) / (Dimension l' w' t' c' te' lu' su') =
>     Dimension (l/l') (w/w') (t/t') (c/c') (te/te') (lu/lu') (su / su') 
>   recip a = fromInteger 1 / a
>   fromRational i = Dimension i i i i i i i 

>instance InnerProductSpace Dimension where
>   (Dimension l w t c te lu su) %. (Dimension l' w' t' c' te' lu' su')
>     = l*l'+w*w'+t*t'+c*c'+te*te'+lu*lu'+su*su'

>instance Semigroup Dimension where
>   (<>) = (%+)

>instance Monoid Dimension where
>   mempty = dimensionless
>   mappend = (%+)

>instance Group Dimension where
>   ginvert = vnegate


>instance VectorSpace Dimension where
>   type Scalar Dimension = Rational
>   vzero = dimensionless
>   vnegate a = mapDimension negate a
>   a %+ b = zipWithDimension (+) a b
>   k %* a = mapDimension (k *) a

>(=*) :: (VectorSpace v) => Scalar v -> v -> v
>(=*) = (%*)

>deca,hecto,kilo,mega,giga,tera,peta,exa,zetta,yotta :: (VectorSpace u, Num (Scalar u)) => u -> u
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

>prefix_value :: (Num a) => Prefix a -> a
>prefix_value f = value_amount $ f $ 1 `As` dimensionless

>decimal_prefixes :: (Num a) => [(String,a)]
>decimal_prefixes = [
>   ("deca",prefix_value deca),
>   ("hecto",prefix_value hecto),
>   ("kilo",prefix_value kilo),
>   ("mega",prefix_value mega),
>   ("giga",prefix_value giga),
>   ("tera",prefix_value tera),
>   ("peta",prefix_value peta),
>   ("exa", prefix_value exa),
>   ("zetta", prefix_value zetta),
>   ("yotta", prefix_value yotta)]

>floating_prefixes :: (Floating a) => [(String,a)]
>floating_prefixes = decimal_prefixes ++ [ 
>   ("deci",prefix_value deci ),
>   ("centi",prefix_value centi ),
>   ("milli",prefix_value milli),
>   ("micro",prefix_value micro),
>   ("nano",prefix_value nano ),
>   ("pico",prefix_value pico ),
>   ("femto",prefix_value femto ),
>   ("atto", prefix_value atto),
>   ("zepto",prefix_value zepto ),
>   ("yocto",prefix_value yocto)]
>                    
>binary_prefixes :: (Num a) => [(String,a)]
>binary_prefixes = [
>   ("kibi",prefix_value kibi ),
>   ("mebi",prefix_value mebi ),
>   ("gibi",prefix_value gibi ),
>   ("tebi",prefix_value tebi ),
>   ("pebi",prefix_value pebi ),
>   ("exbi",prefix_value exbi ),
>   ("zebi",prefix_value zebi ),
>   ("yobi",prefix_value yobi )
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

>deci,centi,milli,micro,nano,pico,femto,atto,zepto,yocto :: (VectorSpace u, Floating (Scalar u)) => u -> u
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

>(=+=) :: (VectorSpace v) => v -> v -> v
>(=+=) = (%+)

>(=-=) :: (VectorSpace v) => v -> v -> v
>x =-= y = x %+ vnegate y

>instance Foldable Quantity where
>   foldMap f (x `As` d) = f x

>instance Traversable Quantity where
>   traverse f (fa `As` d) = (`As` d) <$> f fa

>instance (Show r) => Stream.Limiting (Quantity r) where
>   data Closure (Quantity r) = QuantityClosure (Stream r) Dimension
>   limit z@(Stream.Pre (x `As` d) r@(Stream.Pre (y `As` d') _))
>     | d == d' = let QuantityClosure yr _ = limit r in QuantityClosure (Stream.Pre x yr) d
>     | otherwise = invalidDimensions "limit" d d' x y
>   approximations (QuantityClosure a d) = fmap (`As` d) a

>instance (ShowPrecision r, Floating r, Ord r) => Show (Stream.Closure (Quantity r)) where
>   show (QuantityClosure s d) = show d ++ ":" ++ show s

>instance (Show r,Fractional r) => Fractional (Quantity r) where
>   (x `As` r) / (y `As` r') = (x/y) `As` (r %- r')
>   recip (x `As` r) = (1/x) `As` (vnegate r)
>   fromRational x = fromRational x `As` vzero

>require_dimensionless :: (Show a) => String -> (a -> a) -> Quantity a -> Quantity a
>require_dimensionless op f ~(x `As` r)
>   | isDimensionless r = f x `As` dimensionless
>   | otherwise = invalidDimensions op r dimensionless x (f x)

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
>     | d == d' = atan2 x y `As` radian_dimension
>     | otherwise = invalidDimensions "atan2" d d' x y

>instance (Show r, ConjugateSymmetric r) => ConjugateSymmetric (Quantity r) where
>   conj (x `As` d) = conj x `As` d

>instance (Show r,Floating r, Real r) => Floating (Quantity r) where
>   pi = pi `As` radian_dimension
>   sqrt (x `As` r) = sqrt x `As` (r / 2)
>   (x `As` r) ** (y `As` q)
>     | isDimensionless q = (x ** y) `As` (toRational y %* r)
>     | otherwise = invalidDimensions "**" q dimensionless y y
>   exp = require_dimensionless "exp" exp
>   log = require_dimensionless "log" log
>   sin = require_dimensionless "sin" sin
>   cos = require_dimensionless "cos" cos
>   tan = require_dimensionless "tan" tan
>   asin = require_dimensionless "asin" asin
>   acos = require_dimensionless "acos" acos
>   atan = require_dimensionless "atan" atan
>   sinh = require_dimensionless "sinh" sinh
>   cosh = require_dimensionless "cosh" cosh
>   tanh = require_dimensionless "tanh" tanh
>   asinh = require_dimensionless "asinh" asinh
>   acosh = require_dimensionless "acosh" acosh
>   atanh = require_dimensionless "atanh" atanh

>instance (Ord r, Num r, Show r) => Ord (Quantity r) where
>   compare (x `As` r) (y `As` r')
>     | r == r' = compare (x-y) 0
>     | otherwise = invalidDimensions "compare" r r' x y

>instance (Show r,RealFrac r) => RealFrac (Quantity r) where
>   properFraction (x `As` r) = let (b,c) = properFraction x in (b,c `As` r)
>   round (x `As` r) = round x
>   truncate (x `As` r) = truncate x
>   ceiling (x `As` r) = ceiling x
>   floor (x `As` r) = floor x

>instance (Show r, Real r) => Real (Quantity r) where
>   toRational (x `As` r) = toRational x

>readprefix :: TRead.ReadPrec (Quantity Double -> Quantity Double)
>readprefix = choose $ map (\(a,b) -> (a,return b)) siPrefixes

>readunit :: TRead.ReadPrec (String,Quantity Double)
>readunit = choose $ map (\ (a,b) -> (a,return (a,b))) siUnits

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
>  show (x `As` d) = show_at_precision x 10 ++ " " ++ show d

>instance (ShowPrecision r) => PpShow (Quantity r) where
>  pp (x `As` d) = pp (show_at_precision x 10) Pretty.<+> pp d


>invalidDimensions :: (Show b, Show c) => String -> Dimension -> Dimension -> b -> c -> a
>invalidDimensions op r r' a b= throw $ InvalidDimensionsException r r' $
>     op ++ ":Invalid dimensions: " ++ show r ++ " != " ++ show r' ++ "; " ++ show a ++ "<>" ++ show b
>invalidDimensionsM :: (Monad m, Show b, Show c) => String -> Dimension -> Dimension -> b -> c -> m a
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

>plusQ :: (Monad m, Num r, Show r) => Quantity r -> Quantity r -> m (Quantity r)
>plusQ (x `As` r) (y `As` r')
>   | r /= r' = invalidDimensionsM "plusQ" r r' x y
>   | otherwise = return $! (x+y) `As` r
>
>minusQ :: (Monad m, Num r, Show r) => Quantity r -> Quantity r -> m (Quantity r)
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



>show_dimension :: Dimension -> String
>show_dimension (Dimension a b c d e f g) = if str /= "" then take (length str - 1) str else ""
> where str = showz "A" d ++ showz "kg" b ++ showz "m" a ++  showz "s" c ++ showz "K" e ++ showz "cd" f ++ showz "mol" g
>       showz u 0 = ""
>       showz u 1 = u ++ " "
>       showz u i
>           | denominator i == 1 = u ++ "^" ++ show (numerator i) ++ " "
>           | otherwise = u ++ "^(" ++ show i ++ ") "

>full_dimension_table :: [(Dimension,String)]
>full_dimension_table = res4
>  where res4 = dimension_table `interleave` map mapper dimension_table
>        mapper (dim,p) = (vnegate dim, p ++ "^-1")

order in the table is significant

>dimension_table :: [(Dimension,String)]
>dimension_table = [
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
>   readPrec = do lst <- dimension_product_reader
>                 lst' <- ((optional_whitespace >> string "/" >> dimension_divider_reader) >>= (return . Just)) TRead.<++ return Nothing
>                 let lst'' = maybe lst (\lst2 -> lst ++ map (\j -> negate j) lst2) lst'
>                 return $! sum lst''

>dimension_product_reader :: TRead.ReadPrec [Dimension]
>dimension_product_reader = TRead.step $ do
>                                     v <- dimension_reader
>                                     optional_whitespace
>                                     string "*"
>                                     optional_whitespace
>                                     r <- dimension_product_reader
>                                     return $! (v:r)
>                            TRead.<++ some dimension_reader
>                            TRead.<++ TRead.parens dimension_product_reader

>dimension_divider_reader :: TRead.ReadPrec [Dimension]
>dimension_divider_reader = do
>        x <- dimension_reader
>        whitespace
>        string "/"
>        xr <- dimension_divider_reader
>        return $! (x:xr)
>  TRead.<++ (dimension_reader >>= \x -> return $! [x])
>  TRead.<++ TRead.parens dimension_product_reader

>dimension_reader :: TRead.ReadPrec Dimension
>dimension_reader = choose $ fmap (\ (d,s) -> (s,return d)) full_dimension_table

>dimensionless :: Dimension
>dimensionless = Dimension 0 0 0 0 0 0 0 


>instance PpShow Dimension where
>   pp z@(Dimension a b c d e f g) = maybe (def z) id $ foldl check Nothing full_dimension_table
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
>           def z = pp_dimension z

>-- | pp_dimension prints dimension in a simple way without interpreting derived dimensions
>-- for example, @pp_dimension newton_dimension == pp "kg m s^-2"@
>pp_dimension :: Dimension -> Pretty.Doc
>pp_dimension (Dimension a b c d e f g) = def b "kg" Pretty.<+> def a "m"
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
>  show z@(Dimension a b c d e f g) = maybe (def z) id $ foldl check Nothing full_dimension_table
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
>          def z = show_dimension z

>kelvin_dimension :: Dimension
>kelvin_dimension = Dimension 0 0 0 0 1 0 0 

>farad_dimension :: Dimension
>farad_dimension  = Dimension (-2) (-1) 4 2 0 0 0 
>ohm_dimension :: Dimension
>ohm_dimension    = Dimension 2 1 (-3) (-2) 0 0 0 
>sievert_dimension :: Dimension
>sievert_dimension = Dimension (-2) (-1) 3 2 0 0 0 
>volt_dimension :: Dimension
>volt_dimension    = Dimension 2 1 (-3) (-1) 0 0 0 

>pascal_dimension :: Dimension
>pascal_dimension  = Dimension (-1) 1 (-2) 0 0 0 0 

>henry_dimension :: Dimension
>henry_dimension   = Dimension 2 1 (-2) (-2) 0 0 0 

>weber_dimension :: Dimension
>weber_dimension   = Dimension 2 1 (-2) (-1) 0 0 0 
> 
>joule_dimension :: Dimension
>joule_dimension   = Dimension 2 1 (-2) 0 0 0 0 
> 
>newton_dimension :: Dimension
>newton_dimension  = Dimension 1 1 (-2) 0 0 0 0 
> 
>tesla_dimension :: Dimension
>tesla_dimension   = Dimension 0 1 (-2) (-1) 0 0 0 
> 
>meter_dimension :: Dimension
>meter_dimension    = Dimension 1 0 0 0 0 0 0 
> 
>kilogram_dimension :: Dimension
>kilogram_dimension = Dimension 0 1 0 0 0 0 0 
> 
>second_dimension :: Dimension
>second_dimension   = Dimension 0 0 1 0 0 0 0 
> 
>ampere_dimension :: Dimension
>ampere_dimension   = Dimension 0 0 0 1 0 0 0 
> 
>candela_dimension :: Dimension
>candela_dimension  = Dimension 0 0 0 0 0 1 0 
> 
>mol_dimension :: Dimension
>mol_dimension = Dimension 0 0 0 0 0 0 1  

>-- | radian_dimension is basically same as dimensionless, using separate name anyway
>-- to allow clients to distinguish. No checking for this distinction is implemented.
>radian_dimension :: Dimension
>radian_dimension = meter_dimension %- meter_dimension
>
>-- | steradian_dimension is basically same as dimensionless, redefining anyway
>-- to allow clients to distinguish steradians. No checking for this distinction is implemented.
>steradian_dimension :: Dimension
>steradian_dimension = (2 %* meter_dimension) %- (2 %* meter_dimension)

>steradian :: (Floating a) => Quantity a
>steradian = 1 @@ steradian_dimension
>becquerel_dimension :: Dimension
>becquerel_dimension = 0 %- second_dimension
>becquerel :: (Floating a) => Quantity a
>becquerel = 1 @@ becquerel_dimension
>gray_dimension :: Dimension
>gray_dimension = (2 %* meter_dimension) %- (2 %* second_dimension)
>gray :: (Floating a) => Quantity a
>gray = 1 @@ gray_dimension
>lux_dimension = candela_dimension %- (2 %* meter_dimension)
>lux :: (Floating a) => Quantity a
>lux = 1 @@ lux_dimension

>lumen_dimension :: Dimension
>lumen_dimension = candela_dimension %+ steradian_dimension
>lumen :: (Floating a) => Quantity a
>lumen = 1 @@ lumen_dimension
>kelvin_quantity :: (Floating a) => Quantity a
>kelvin_quantity = 1 @@ kelvin_dimension

>siemens :: (Floating a) => Quantity a
>siemens = 1 @@ siemens_dimension

>second :: (Floating a) => Quantity a
>second = 1.0 @@ second_dimension

>meter :: (Floating a) => Quantity a
>meter = 1.0 @@ meter_dimension
>
>kilogram :: (Floating a) => Quantity a
>kilogram = 1.0 @@ kilogram_dimension
>
>gram :: (Floating a) => Quantity a
>gram = milli kilogram
> 
>mole :: (Floating a) => Quantity a
>mole = 1.0 @@ mol_dimension
>candela :: (Floating a) => Quantity a 
>candela = 1.0 @@ candela_dimension
>kelvin :: (Floating a) => Quantity a
>kelvin = 1.0 @@ kelvin_dimension
>ampere :: (Floating a) => Quantity a
>ampere = 1.0 @@ ampere_dimension
>coulomb :: (Floating a) => Quantity a
>coulomb = 1.0 @@ coulomb_dimension
>tesla :: (Floating a) => Quantity a
>tesla = 1.0 @@ tesla_dimension

>henry :: (Floating a) => Quantity a
>henry = 1.0 @@ henry_dimension

>newton :: (Floating a) => Quantity a
>newton = 1.0 @@ newton_dimension

>pascal :: (Floating a) => Quantity a
>pascal = 1.0 @@ pascal_dimension

>joule :: (Floating a) => Quantity a
>joule = 1.0 @@ joule_dimension

>weber :: (Floating a) => Quantity a
>weber = 1.0 @@ weber_dimension

>volt :: (Floating a) => Quantity a
>volt = 1.0 @@ volt_dimension

>sievert :: (Floating a) => Quantity a
>sievert = 1.0 @@ sievert_dimension

>ohm :: (Floating a) => Quantity a
>ohm = 1.0 @@ ohm_dimension

>farad :: (Floating a) => Quantity a
>farad = 1.0 @@ farad_dimension

>watt :: (Floating a) => Quantity a
>watt = 1.0 @@ watt_dimension

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
>radian = 1.0 @@ radian_dimension
>katal_dimension :: Dimension
>katal_dimension = mol_dimension %- second_dimension
>katal :: (Floating a) => Quantity a
>katal = 1.0 @@ katal_dimension

>-- | conversion from degrees celcius.
>-- WARNING: produces degrees in kelvin, so computations intended
>-- in degrees celcius should be done before conversion!
>fromCelsius :: (Show a, Fractional a) => a -> Quantity a
>fromCelsius x = x @@ kelvin_dimension + (273.15 @@ kelvin_dimension)

>fromFahrenheit :: (Fractional a, Show a) => a -> Quantity a
>fromFahrenheit x = ((x + 459.67) * (5/9)) @@ kelvin_dimension

>toFahrenheit :: (Monad m, Fractional a,VectorSpace a)
> => Quantity a -> m a
>toFahrenheit x
>   | dimension x == kelvin_dimension = return $! (amount x * (9/5)) - 459.67
>   | otherwise = fail "Cannot convert to fahrenheit"

>toCelsius :: (Fractional a, Show a, VectorSpace a) => Quantity a -> a
>toCelsius x = amount (x - 273.15 @@ kelvin_dimension)

>fromRadiansAngle :: (Floating a) => a -> Quantity a
>fromRadiansAngle r = r `As` radian_dimension

>-- | conversion from angle.
>-- WARNING: produces degrees in radian, so computations intended
>-- in degrees should be done before conversion!
>fromDegreesAngle :: (Floating a) => a -> Quantity a
>fromDegreesAngle alfa = (alfa * pi / 180.0) `As` radian_dimension

>hertz_dimension :: Dimension
>hertz_dimension = dimensionless %- second_dimension
>hertz :: (Floating a) => Quantity a
>hertz = 1.0 @@ hertz_dimension

>squaremeter_dimension :: Dimension
>squaremeter_dimension = 2 %* meter_dimension

>squaremeter :: (Floating a) => Quantity a
>squaremeter = 1.0 @@ squaremeter_dimension
>
>cubicmeter_dimension :: Dimension
>cubicmeter_dimension = 3 %* meter_dimension
>cubicmeter :: (Floating a) => Quantity a
>cubicmeter = 1.0 @@ cubicmeter_dimension

>watt_dimension :: Dimension
>watt_dimension = (kilogram_dimension %+ (2 %* meter_dimension) %- (3 %* second_dimension))
>coulomb_dimension :: Dimension
>coulomb_dimension = (second_dimension %+ ampere_dimension)
>siemens_dimension :: Dimension
>siemens_dimension = (3 %* second_dimension) %+ (2 %* ampere_dimension)
>                 %- (kilogram_dimension %+ squaremeter_dimension)


>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>planck_length :: (Floating a) => Quantity a
>planck_length = 1.61619997e-35 %* meter
>
>planck_mass :: (Floating a) => Quantity a
>planck_mass = 2.1765113e-8 %* kilogram
>planck_time :: (Floating a) => Quantity a
>planck_time = 5.3910632e-44 %* second
>planck_charge :: (Floating a) => Quantity a
>planck_charge = 1.87554595641e-18 %* coulomb
>planck_temperature :: (Floating a) => Quantity a
>planck_temperature = 1.41683385e32 %* kelvin

>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>--  <https://en.wikipedia.org/wiki/2019_redefinition_of_SI_base_units>
>-- Warning: don't confuse with stefan_boltzmann_constant.
>-- Notice value is exact.
>boltzmann_constant :: (Floating a) => Quantity a
>boltzmann_constant = 1.380649e-23 @@ (joule_dimension %- kelvin_dimension)

>-- | <https://en.wikipedia.org/wiki/Gravitational_constant>
>gravitational_constant :: (Floating a) => Quantity a
>gravitational_constant = 6.6740831e-11 @@ (3 %* meter_dimension %+ (negate 1) %* kilogram_dimension %+ (negate 2) %* second_dimension)
>
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>standard_acceleration_of_gravity :: (Floating a) => Quantity a
>standard_acceleration_of_gravity = 9.80665 @@ (meter_dimension %- 2 %* second_dimension)
> 
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>standard_atmosphere :: (Floating a) => Quantity a
>standard_atmosphere = 101325 %* pascal

>-- | <https://en.wikipedia.org/wiki/Speed_of_light>
>-- Note value is exact.
>speed_of_light :: (Floating a) => Quantity a
>speed_of_light = 299792458.0 @@ (meter_dimension %- second_dimension)

>-- | <https://en.wikipedia.org/wiki/Planck_constant>
>-- <https://en.wikipedia.org/wiki/2019_redefinition_of_SI_base_units>
>-- Note value is exact.
>planck_constant :: (Floating a) => Quantity a
>planck_constant = 6.62607015e-34 @@ (joule_dimension %+ second_dimension)

>reduced_planck_constant :: (Floating a) => Quantity a
>reduced_planck_constant = (1/(2*pi)) %* planck_constant

>-- | <https://en.wikipedia.org/wiki/Electronvolt>
>electronvolt :: (Floating a) => Quantity a
>electronvolt = 1.6021766208e-19 @@ joule_dimension

>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>magnetic_constant :: (Floating a) => Quantity a
>magnetic_constant = (4*pi*1e-7) @@ (newton_dimension %- (2 %* ampere_dimension))
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>electric_constant :: (Show a, Floating a) => Quantity a
>electric_constant = 1 / (magnetic_constant * speed_of_light * speed_of_light)
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>characteristic_impedance_of_vacuum :: (Floating a) => Quantity a
>characteristic_impedance_of_vacuum = 376.730313461 @@ ohm_dimension
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>coulombs_constant :: (Floating a) => Quantity a
>coulombs_constant = 8.9875517873681764e9 @@ (kilogram_dimension %+ (3 %* meter_dimension) %- (4 %* second_dimension) %- (2 %* ampere_dimension))
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>--   <https://en.wikipedia.org/wiki/2019_redefinition_of_SI_base_units>
>--   Notice value is exact.
>elementary_charge :: (Floating a) => Quantity a
>elementary_charge = 1.602176634e-19 @@ coulomb_dimension

>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>bohr_magneton :: (Floating a) => Quantity a
>bohr_magneton = 9.27400999457e-24 @@ (joule_dimension %- tesla_dimension)
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>conductance_quantum :: (Floating a) => Quantity a
>conductance_quantum = 7.748091731018e-5 @@ sievert_dimension
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>inverse_conductance_quantum :: (Floating a) => Quantity a
>inverse_conductance_quantum = 12906.403727829 @@ ohm_dimension

>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>josephson_constant :: (Floating a) => Quantity a
>josephson_constant = 4.83597852530e14 @@ (hertz_dimension %- volt_dimension)
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>magnetic_flux_quantum :: (Floating a) => Quantity a
>magnetic_flux_quantum = 2.06783383113e-15 @@ weber_dimension
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>nuclear_magneton :: (Floating a) => Quantity a
>nuclear_magneton = 5.05078369931e-27 @@ (joule_dimension %- tesla_dimension)
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>von_klitzing_constant :: (Floating a) => Quantity a
>von_klitzing_constant = 25812.807455559 @@ ohm_dimension
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>bohr_radius :: (Floating a) => Quantity a
>bohr_radius = 5.291722106712e-11 @@ meter_dimension
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>classical_electron_radius :: (Floating a) => Quantity a
>classical_electron_radius = 2.817940322719e-15 @@ meter_dimension
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>electron_mass :: (Floating a) => Quantity a
>electron_mass = 9.1093835611e-31 @@ kilogram_dimension
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>fermi_coupling_constant :: (Show a,Floating a) => Quantity a
>fermi_coupling_constant = 1.16637876e-5 %* giga (1 / (electronvolt * electronvolt))
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>fine_structure_constant :: (Floating a) => Quantity a
>fine_structure_constant = 7.297352566417e-3 @@ dimensionless

>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>hartree_energy :: (Floating a) => Quantity a
>hartree_energy = 4.35974465054e-18 @@ joule_dimension

>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>proton_mass :: (Floating a) => Quantity a
>proton_mass = 1.67262189821e-27 @@ kilogram_dimension
>
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>quantum_of_circulation :: (Floating a) => Quantity a
>quantum_of_circulation = 3.636947548617e-4 @@ (kilogram_dimension %+ kilogram_dimension %- second_dimension)
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>rydberg_constant :: (Floating a) => Quantity a
>rydberg_constant = 10973731.56850865 @@ ((negate 1) %* meter_dimension)
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>thomson_cross_section :: (Floating a) => Quantity a
>thomson_cross_section = 6.652458715891e-29 @@ (2 %* meter_dimension)

>-- <https://en.wikipedia.org/wiki/2019_redefinition_of_SI_base_units>
>ground_state_cesium_hyperfine_transition_frequency :: (Floating a) => Quantity a
>ground_state_cesium_hyperfine_transition_frequency = 9192631770 @@ hertz_dimension

>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>weak_mixing_angle :: (Floating a) => Quantity a
>weak_mixing_angle = 0.222321 @@ radian_dimension

>-- | <https://en.wikipedia.org/wiki/2019_redefinition_of_SI_base_units>
>-- value is exact.
>luminous_efficacy_of_monochromatic_540e12_hertz_radiation :: (Floating a) => Quantity a
>luminous_efficacy_of_monochromatic_540e12_hertz_radiation = 683 @@ (lumen_dimension %- watt_dimension)

>-- | <https://en.wikipedia.org/wiki/Avogadro_constant>
>-- <https://en.wikipedia.org/wiki/2019_redefinition_of_SI_base_units>
>-- Notice value is exact.
>avogadro_constant :: (Floating a) => Quantity a
>avogadro_constant = 6.02214076e23 @@ (negate $ mol_dimension)
>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>atomic_mass_constant :: (Floating a) => Quantity a
>atomic_mass_constant = 1.66053904020e-27 %* kilogram

>faraday_constant :: (Floating a, Show a) => Quantity a
>faraday_constant = 96485.3328959 %* (coulomb / mole)

>first_radiation_constant :: (Floating a, Show a) => Quantity a
>first_radiation_constant = 1.19104295315e-16 %* (watt * meter * meter)
>loschmidt_constant :: (Floating a) => Quantity a
>loschmidt_constant = 2.686781115e25 @@ ((-3) %* meter_dimension)
>gas_constant :: (Floating a, Show a) => Quantity a
>gas_constant = 8.314459848 %* (joule / (mole * kelvin))
>molar_planck_constant :: (Floating a, Show a) => Quantity a
>molar_planck_constant = 3.990312711018e-10 %* (joule * second / mole)
>second_radiation_constant :: (Floating a, Show a) => Quantity a
>second_radiation_constant = 1.4387773683e-2 %* (meter * kelvin)

>-- | Warning: don't confuse with boltzmann_constant.
>stefan_boltzmann_constant :: (Floating a, Show a) => Quantity a
>stefan_boltzmann_constant = 5.67036713e-8 %* (watt / (meter^2 * kelvin^4))

>-- | <https://en.wikipedia.org/wiki/Physical_constant>
>efimov_factor :: (Floating a) => Quantity a 
>efimov_factor = 22.7 @@ dimensionless
>
>-- | <https://en.wikipedia.org/wiki/Light-year>
>lightyear :: (Num a) => Quantity a
>lightyear = 9460730472580800 @@ meter_dimension

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
>  ("eV", joule_dimension),
>  ("m", meter_dimension),
>  ("rad", radian_dimension),
>  ("sr", steradian_dimension),
>  ("K", kelvin_dimension),
>  ("s", second_dimension),
>  ("kg", kilogram_dimension),
>  ("g", kilogram_dimension),
>  ("mol", mol_dimension),
>  ("cd", candela_dimension),
>  ("A", ampere_dimension),
>  ("C", coulomb_dimension),
>  ("T", tesla_dimension),
>  ("H", henry_dimension),
>  ("N", newton_dimension),
>  ("Pa", pascal_dimension),
>  ("J", joule_dimension),
>  ("Wb", weber_dimension),
>  ("V", volt_dimension),
>  ("Sv", sievert_dimension),
>  ("S", siemens_dimension),
>  ("Ω", ohm_dimension),
>  ("ohm", ohm_dimension),
>  ("F", farad_dimension),
>  ("W", watt_dimension),
>  ("°C", kelvin_dimension),
>  ("°F", kelvin_dimension),
>  ("°", radian_dimension),
>  ("lm", lumen_dimension),
>  ("lx", lux_dimension),
>  ("Bq", becquerel_dimension),
>  ("Gy", gray_dimension),
>  ("kat", katal_dimension),
>  ("Hz", hertz_dimension),
>  ("ly", meter_dimension),
>  ("AU", meter_dimension),
>  ("pc", meter_dimension),
>  ("l", 3 %* meter_dimension),
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
>siPrefixes = [("Y", yotta),
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
