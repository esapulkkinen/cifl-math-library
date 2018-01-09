>{-# LANGUAGE Safe, GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
>module Math.Matrix.Dimension where
>import Data.Complex
>import Math.Tools.List (interleave)
>import Math.Matrix.Interface
>import Math.Number.Stream (Stream, Limiting(..), Closed(..))
>import qualified Math.Number.Stream as Stream

>import Math.Number.Group
>import Math.Number.Real

>type Rep = R

>data Quantity r = As { value_amount :: r,
>                       value_dimension :: Dimension }
>  deriving (Eq)

>instance (Show r, Infinitesimal r) => Infinitesimal (Quantity r) where
>   epsilon = limit $ fmap (`As` dimensionless) epsilon_stream

>mapQuantity :: (a -> b)
>            -> (Dimension -> Dimension)
>            -> Quantity a -> Quantity b
>mapQuantity f g (r `As` d) = f r `As` g d

>complexQuantity :: Complex (Quantity r) -> Quantity (Complex r)
>complexQuantity ((a `As` d) :+ (b `As` d'))
>   | d == d' = (a :+ b) `As` d
>   | otherwise = error "Mismatching dimensions in complexQuantity"

>quantityComplex :: Quantity (Complex r) -> Complex (Quantity r)
>quantityComplex ((a :+ b) `As` d) = (a `As` d) :+ (b `As` d)

>instance (Closed a) => Closed (Quantity a) where
>   accumulation_point s
>     | (Stream.Pre (As x d) xr) <- approximations s
>     = accumulation_point (limit $ Stream.Pre x (fmap value_amount xr)) `As` d

>data Level r = Level {
>   reference_value :: Quantity r,
>   base_of_logarithm :: r }

>level :: (Show a, Floating a, Real a) => Quantity a -> Level a -> Quantity a
>level x (Level q b) = log (x / q) / (log b `As` dimensionless)

>class (VectorSpace u) => Unit u where
>   amount :: u -> Scalar u
>   fromAmount :: Scalar u -> u
>   unitOf :: u -> String
>   dimension :: u -> Dimension

>quantity :: (Unit u) => u -> Quantity (Scalar u)
>quantity x = amount x @@ dimension x

>convert :: (Scalar u ~ Scalar v, Unit v, Unit u, Show v, Show u, Monad m, Fractional (Scalar u))
>        => v -> u -> m u
>convert x d
>   | dimension x == dimension d = return ((amount x / amount d) =* d)
>   | otherwise = invalidDimensionsM "convert" (dimension x) (dimension d) x d

>(=/) :: (Fractional (Scalar a), Unit a) => a -> a -> Scalar a
>(=/) x y = amount x / amount y


>data Dimension = Dimension {
>   length_power :: Rational,
>   weight_power :: Rational,
>   time_power   :: Rational,
>   current_power :: Rational,
>   temperature_power :: Rational,
>   luminosity_power  :: Rational,
>   substance_power   :: Rational }
>  deriving (Eq)

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
>   fromRational i = Dimension 0 0 0 0 0 0 0 

>instance InnerProductSpace Dimension where
>   (Dimension l w t c te lu su) %. (Dimension l' w' t' c' te' lu' su')
>     = l*l'+w*w'+t*t'+c*c'+te*te'+lu*lu'+su*su'

>instance Monoid Dimension where
>   mempty = dimensionless
>   mappend = (%+)

>instance Group Dimension where
>   ginvert = vnegate

>instance VectorSpace Dimension where
>   type Scalar Dimension = Rational
>   vzero = Dimension 0 0 0 0 0 0 0 
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

>instance Stream.Limiting (Quantity r) where
>   data Closure (Quantity r) = QuantityClosure (Stream (Quantity r))
>   limit s = QuantityClosure s
>   approximations (QuantityClosure a) = a

>instance (ShowPrecision r, Floating r, Ord r) => Show (Stream.Closure (Quantity r)) where
>   show (QuantityClosure s) = show s

>instance (Show r,Fractional r) => Fractional (Quantity r) where
>   (x `As` r) / (y `As` r') = (x/y) `As` (r %- r')
>   recip (x `As` r) = (1/x) `As` (vnegate r)
>   fromRational x = fromRational x `As` vzero

>require_dimensionless :: (a -> a) -> Quantity a -> Quantity a
>require_dimensionless f (x `As` r)
>   | isDimensionless r = f x `As` dimensionless
>   | otherwise = error "invalid dimension as input to function"

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
>     | d == d' = atan2 x y `As` d
>     | otherwise = error "mismatching dimensions"

>instance (Show r,Floating r, Real r) => Floating (Quantity r) where
>   pi = pi `As` radian_dimension
>   sqrt (x `As` r) = sqrt x `As` (r / 2)
>   (x `As` r) ** (y `As` q)
>     | isDimensionless q = (x ** y) `As` (toRational y %* r)
>     | otherwise = error "Cannot raise to dimensional power"
>   exp = require_dimensionless exp
>   log = require_dimensionless log
>   sin = require_dimensionless sin
>   cos = require_dimensionless cos
>   tan = require_dimensionless tan
>   asin = require_dimensionless asin
>   acos = require_dimensionless acos
>   atan = require_dimensionless atan
>   sinh = require_dimensionless sinh
>   cosh = require_dimensionless cosh
>   tanh = require_dimensionless tanh
>   asinh = require_dimensionless asinh
>   acosh = require_dimensionless acosh
>   atanh = require_dimensionless atanh

>instance (Ord r) => Ord (Quantity r) where
>   compare (x `As` r) (y `As` r') | r == r' = compare x y
>                                  | otherwise = error "Invalid comparison"

>instance (Show r,RealFrac r) => RealFrac (Quantity r) where
>   properFraction (x `As` r) = let (b,c) = properFraction x in (b,c `As` r)
>   round (x `As` r) = round x
>   truncate (x `As` r) = truncate x
>   ceiling (x `As` r) = ceiling x
>   floor (x `As` r) = floor x

>instance (Show r, Real r) => Real (Quantity r) where
>   toRational (x `As` r) = toRational x

>instance (Num r, VectorSpace r) => Unit (Quantity r) where
>  amount (x `As` _) = x
>  unitOf (_ `As` r) = show r
>  dimension (_ `As` r) = r
>  fromAmount x = x `As` dimensionless

>instance (ShowPrecision r, Floating r, Ord r) => Show (Quantity r) where
>  show (x `As` d)
>   | d == kelvin_dimension && x >= 200 = show_at_precision (x-273.15) 10 ++ " ⁰C"
>   | otherwise = show_at_precision x 10 ++ " " ++ show d


>(@@) :: r -> Dimension -> Quantity r
>x @@ y = x `As` y

>invalidDimensions :: (Show b, Show c) => String -> Dimension -> Dimension -> b -> c -> a
>invalidDimensions op r r' a b= error $
>     op ++ ":Invalid dimensions: " ++ show r ++ " != " ++ show r' ++ "; " ++ show a ++ "<>" ++ show b
>invalidDimensionsM :: (Monad m, Show b, Show c) => String -> Dimension -> Dimension -> b -> c -> m a
>invalidDimensionsM op r r' a b = fail $
>     op ++ "Invalid dimensions: " ++ show r ++ " != " ++ show r' ++ "; " ++ show a ++ "<>" ++ show b

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
>      | otherwise = error $ "Vector sum: Invalid dimensions: " ++ show d ++ " != " ++ show d'
>   a %* (x `As` d) = (a * x) `As` d

>instance (Num r, NormedSpace r) => NormedSpace (Quantity r) where
>   norm (x `As` _) = x

>show_dimension :: Dimension -> String
>show_dimension (Dimension a b c d e f g) = showz "m" a ++ showz "kg" b ++ showz "s" c ++ showz "A" d ++ showz "K" e ++ showz "cd" f ++ showz "mol" g 
> where  showz u 0 = "" 
>        showz u i = u ++ "^(" ++ show i ++ ")"

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


>dimensionless :: Dimension
>dimensionless = Dimension 0 0 0 0 0 0 0 

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
>                            = Just (p ++ " " ++ show (z %- z'))
>                    | otherwise = Nothing
>          check (Just x) _ = Just x                      
>          def (Dimension 0 0 0 0 0 0 0) = ""
>          def z = show_dimension z

>mapAmount :: (Unit a) => (Scalar a -> Scalar a) -> a -> a
>mapAmount f = fromAmount . f . amount

>mapAmount2 :: (Unit a) => (Scalar a -> Scalar a -> Scalar a) -> a -> a -> a
>mapAmount2 f x y = fromAmount (f (amount x) (amount y))

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
> 

>bel_dimension :: Dimension
>bel_dimension = Dimension 0 1 (-3) 0 0 0 0 

>radian_dimension :: Dimension
>radian_dimension = meter_dimension %- meter_dimension
>
>steradian_dimension :: Dimension
>steradian_dimension = (2 %* meter_dimension) %- (2 %* meter_dimension)

>lumen_dimension :: Dimension
>lumen_dimension = candela_dimension %+ steradian_dimension

>kelvin_quantity = 1 @@ kelvin_dimension

>second :: (Floating a) => Quantity a
>second = 1.0 @@ second_dimension
>meter :: (Floating a) => Quantity a
>meter = 1.0 @@ meter_dimension
>kilogram :: (Floating a) => Quantity a
>kilogram = 1.0 @@ kilogram_dimension
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

>fromCelsius :: (Show a, Fractional a) => a -> Quantity a
>fromCelsius x = x @@ kelvin_dimension + (273.15 @@ kelvin_dimension)

>hertz_dimension = dimensionless %- second_dimension
>hertz = 1.0 @@ hertz_dimension

>squaremeter_dimension = 2 %* meter_dimension
>squaremeter = 1.0 @@ squaremeter_dimension
>
>cubicmeter_dimension = 3 %* meter_dimension
>cubicmeter = 1.0 @@ cubicmeter_dimension

>watt_dimension = (kilogram_dimension %+ (2 %* meter_dimension) %- (3 %* second_dimension))
>
>coulomb_dimension = (second_dimension %+ ampere_dimension)
>siemens_dimension = (3 %* second_dimension) %+ (2 %* ampere_dimension)
>                 %- (kilogram_dimension %+ squaremeter_dimension)

>desibel :: (Fractional a) => Quantity a
>desibel = 0.1 @@ bel_dimension

https://en.wikipedia.org/wiki/Physical_constant

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

>boltzmann_constant :: (Floating a) => Quantity a
>boltzmann_constant = 1.3808e-23 @@ (joule_dimension %- kelvin_dimension)

>-- | <https://en.wikipedia.org/wiki/Gravitational_constant>
>gravitational_constant :: (Floating a) => Quantity a
>gravitational_constant = 6.6740831e-11 @@ (3 %* meter_dimension %+ (negate 1) %* kilogram_dimension %+ (negate 2) %* second_dimension)
>
>-- | <https://en.wikipedia.org/wiki/Speed_of_light>
>speed_of_light :: (Floating a) => Quantity a
>speed_of_light = 299792458.0 @@ (meter_dimension %- second_dimension)

>-- | <https://en.wikipedia.org/wiki/Planck_constant>
>planck_constant :: (Floating a) => Quantity a
>planck_constant = 6.62607004081e-34 @@ (joule_dimension %+ second_dimension)
>reduced_planck_constant :: (Floating a) => Quantity a
>reduced_planck_constant = (1/(2*pi)) %* planck_constant

>-- | <https://en.wikipedia.org/wiki/Electronvolt>
>electronvolt :: (Floating a) => Quantity a
>electronvolt = 1.6021766208e-19 @@ joule_dimension

>-- | <https://en.wikipedia.org/wiki/Physical_constant>

>magnetic_constant :: (Floating a) => Quantity a
>magnetic_constant = (4*pi*1e-7) @@ (newton_dimension %- (2 %* ampere_dimension))
>electric_constant :: (Show a, Floating a) => Quantity a
>electric_constant = 1 / (magnetic_constant * speed_of_light * speed_of_light)
>characteristic_impedance_of_vacuum :: (Floating a) => Quantity a
>characteristic_impedance_of_vacuum = 376.730313461 @@ ohm_dimension
>coulombs_constant :: (Floating a) => Quantity a
>coulombs_constant = 8.9875517873681764e9 @@ (kilogram_dimension %+ (3 %* meter_dimension) %- (4 %* second_dimension) %- (2 %* ampere_dimension))
>elementary_charge :: (Floating a) => Quantity a
>elementary_charge = 1.602178620898e-19 @@ coulomb_dimension

>bohr_magneton :: (Floating a) => Quantity a
>bohr_magneton = 9.27400999457e-24 @@ (joule_dimension %- tesla_dimension)
>conductance_quantum :: (Floating a) => Quantity a
>conductance_quantum = 7.748091731018e-5 @@ sievert_dimension
>inverse_conductance_quantum :: (Floating a) => Quantity a
>inverse_conductance_quantum = 12906.403727829 @@ ohm_dimension

>josephson_constant :: (Floating a) => Quantity a
>josephson_constant = 4.83597852530e14 @@ (hertz_dimension %- volt_dimension)
>magnetic_flux_quantum :: (Floating a) => Quantity a
>magnetic_flux_quantum = 2.06783383113e-15 @@ weber_dimension
>nuclear_magneton :: (Floating a) => Quantity a
>nuclear_magneton = 5.05078369931e-27 @@ (joule_dimension %- tesla_dimension)
>von_klitzing_constant :: (Floating a) => Quantity a
>von_klitzing_constant = 25812.807455559 @@ ohm_dimension
>bohr_radius :: (Floating a) => Quantity a
>bohr_radius = 5.291722106712e-11 @@ meter_dimension
>classical_electron_radius :: (Floating a) => Quantity a
>classical_electron_radius = 2.817940322719e-15 @@ meter_dimension
>electron_mass :: (Floating a) => Quantity a
>electron_mass = 9.1093835611e-31 @@ kilogram_dimension
>fermi_coupling_constant :: (Show a,Floating a) => Quantity a
>fermi_coupling_constant = 1.16637876e-5 %* giga (1 / (electronvolt * electronvolt))
>fine_structure_constant :: (Floating a) => Quantity a
>fine_structure_constant = 7.297352566417e-3 @@ dimensionless

>hartree_energy :: (Floating a) => Quantity a
>hartree_energy = 4.35974465054e-18 @@ joule_dimension

>proton_mass :: (Floating a) => Quantity a
>proton_mass = 1.67262189821e-27 @@ kilogram_dimension
>
>quantum_of_circulation :: (Floating a) => Quantity a
>quantum_of_circulation = 3.636947548617e-4 @@ (kilogram_dimension %+ kilogram_dimension %- second_dimension)
>rydberg_constant :: (Floating a) => Quantity a
>rydberg_constant = 10973731.56850865 @@ ((negate 1) %* meter_dimension)
>thomson_cross_section :: (Floating a) => Quantity a
>thomson_cross_section = 6.652458715891e-29 @@ (2 %* meter_dimension)

>weak_mixing_angle :: (Floating a) => Quantity a
>weak_mixing_angle = 0.222321 @@ dimensionless

>efimov_factor :: (Floating a) => Quantity a 
>efimov_factor = 22.7 @@ dimensionless


