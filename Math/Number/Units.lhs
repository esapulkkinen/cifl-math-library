>-- -*- coding: utf-8 -*-
>{-# LANGUAGE CPP, Trustworthy, TypeOperators, DataKinds, KindSignatures, PolyKinds #-}
>{-# LANGUAGE FlexibleInstances, TypeFamilyDependencies #-}
>{-# LANGUAGE UndecidableInstances #-}
>{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
>{-# LANGUAGE ExistentialQuantification, TypeFamilies,GADTs, RankNTypes, UnicodeSyntax #-}
>{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, FlexibleContexts, DeriveGeneric, DeriveDataTypeable, DeriveAnyClass #-}

#if __GLASGOW_HASKELL__ >= 806

>{-# LANGUAGE DerivingVia #-}

#endif

> 
>-- | This module contains auxiliary definitions related to dimensional analysis.
>--   This is based on the SI system of units.
>--   This module supports compile-time checking for dimensional units.
>--   This is accomplished by a set of newtype wrappers of Double.
>--
>--   It is not necessarily a good idea to make the checking in compile-time.
>--   In particular, this can prevent operations to be available, e.g. Num 
>--   class is not supported for compile-time checked quantities. Num because it has
>--   wrong type for the multiplication and division. Instead of Num, VectorSpace operations
>--   and operations defined in this module should be used.
>-- 
>--   However, these types can distinguish quantities even in cases where the dimensions
>--   match, e.g. radians and steradians. Explicit conversions are nonetheless available
>--   when dimensions match.
>--
>--   Read operations are pretty strict in that they require exactly
>--   the syntax "<double> <unit>", where <unit> is the result of
>--   @unitOf (fromAmount 0 :: T)@. Example: @(read "3 m^2" :: SquareLength)@
>--   This can sometimes be unintuitive and is sensitive to the algorithm
>--   used to print units.
>--   For example: @unitOf (fromAmount 0 :: Mass :/ Length) == "m^-1 kg"@.
>--   If in doubt, it's possible to use the read algorithm handling all units
>--   from DimensionalAnalysis module.
>--
>--   However, it's very much likely that compile-time checked newtypes
>--   are faster at run-time than using the run-time checked quantities. This is probably
>--   the primary reason for wanting this module instead of run-time checked version.
>-- 
>--   Also it's not possible to cast input data directly without run-time check
>--   to a compile-time checked quantity.
>--
>--   See "Barton&Nackman: Scientific and Engineering C++" for C++ approach
>--   to dimensional analysis.
>--  
>--   <https://en.wikipedia.org/wiki/International_System_of_Units>
>module Math.Number.Units where
>import safe Control.Monad (guard)
>import safe Control.Applicative
>import safe Data.Typeable
>import safe Data.Data
>import safe GHC.Generics hiding (R)
>import safe Data.Binary
>import safe Data.Ratio
>import safe Data.Complex
>import Math.Matrix.Interface
>import Math.Number.DimensionalAnalysis
>import Math.Number.TypeRational
>import safe Data.Ratio
>import Math.Number.Real (R)
>import safe qualified Text.ParserCombinators.ReadPrec
>import safe GHC.Read (readPrec)
>import safe Text.ParserCombinators.ReadPrec (lift)
>import safe Text.ParserCombinators.ReadP (skipSpaces, string)
>import Math.Tools.Isomorphism
>import Math.Tools.Arrow
>import qualified Data.Ratio

>-- | Notice that 'Math.Number.DimensionalAnalysis.fromAmount' from Unit class
>-- has type @(Unit u) => UnitName u@.
>-- However usually a concrete constructor of one of the newtypes is used as value.
>type UnitName u = Scalar u -> u

>(*%%) :: (Scalar u ~ Scalar u') => UnitName u -> UnitName u' -> UnitName (u :* u')
>(*%%) a b = \v -> QProduct v a b

>(/%%) :: (Scalar u ~ Scalar u') => UnitName u -> UnitName u' -> UnitName (u :/ u')
>(/%%) a b = \v -> QDivide v a b

>-- | heterogeneous product respecting units
>-- It may be necessary to use 'Math.Number.Units.quantity' operation
>-- to convert to run-time representation after using this, because the result type
>-- accumulates new parts with each use without considering what the total
>-- dimension of the result is. This can produce excessively complicated types, because
>-- the type represents the structure of the expression rather than structure of
>-- the result.
>(*%) :: (LiteralUnit u, LiteralUnit w, Num (Scalar u), Scalar u ~ Scalar w)
>     => u -> w -> u :* w
>a *% b = QProduct (amount a * amount b) fromAmount fromAmount


>-- | heterogeneous division respecting units.
>-- These simply accumulate type information about the units.
>-- It may be necessary to use 'Math.Number.Units.quantity' operation
>-- to convert to run-time representation after using this, because the result type
>-- accumulates new parts with each use without considering what the total
>-- dimension of the result is. This can produce excessively complicated types, because
>-- the type represents the structure of the expression rather than the structure of
>-- the result.
>(/%) :: (Fractional (Scalar u), LiteralUnit u, LiteralUnit w, Scalar u ~ Scalar w)
>     => u -> w -> u :/ w
>a /% b = QDivide (amount a / amount b) fromAmount fromAmount

>unitNameToDimension :: (Num (Scalar u), Unit u) => UnitName u -> Dimension
>unitNameToDimension f = dimension (f 0)

>mapAmount :: (LiteralUnit a) => (Scalar a -> Scalar a) -> a -> a
>mapAmount f = fromAmount . f . amount

>mapAmount2 :: (LiteralUnit a) => (Scalar a -> Scalar a -> Scalar a) -> a -> a -> a
>mapAmount2 f x y = fromAmount (f (amount x) (amount y))
>

>-- | Conversion from "Quantity a" to a unit-specific type.
>-- The second argument is the constructor for the newtype specific to the unit.
>-- 
>-- Example:  @(3 %* meter) `asUnit` Meters == return (Meters 3)@
>--           @0.3 `asUnit` Radians == return (Radians 0.3)@
>--  
>-- If the dimensions don't match, this raises an exception.
>asUnit :: (MonadFail m, Show (Scalar u), Show u, LiteralUnit u, Fractional (Scalar u))
>       => Quantity (Scalar u) -> UnitName u -> m u
>asUnit (x `As` d) f = let v = f (x / conversionFactor f - zeroAmount f)
> in if d == dimension v then return v
>                        else invalidDimensionsM "asUnit" d (dimension v) x (amount v)

>-- | Converts a compile-time checked dimensional unit to run-time checked version
>-- This often has the effect of reducing the complexity in types.
>quantity :: (LiteralUnit u) => u -> Quantity (Scalar u)
>quantity (x :: u) = (conversionFactor (fromAmount :: Scalar u -> u) * (amount x
>                    + zeroAmount (fromAmount :: Scalar u -> u))) @@ dimension x

>data a :/ b  = QDivide { qdivide_amount :: !(Scalar a),
>                         qdivide_dividend_unit :: UnitName a,
>                         qdivide_divisor_unit  :: UnitName b }
>   deriving (Typeable, Generic)

>data a :* b = QProduct { qproduct_amount :: !(Scalar a),
>                         qproduct_first_unit :: UnitName a,
>                         qproduct_second_unit :: UnitName b
>                       }
>   deriving (Typeable, Generic)
>
>instance (LiteralUnit a, LiteralUnit b, Scalar a ~ Scalar b) => VectorSpace (a :* b) where
>   type Scalar (a :* b) = Scalar a
>   vzero = QProduct 0 fromAmount fromAmount
>   vnegate (QProduct x s t) = QProduct (negate x) s t
>   (QProduct d s t) %+ (QProduct d' _ _) = QProduct (d + d') s t
>   k %* (QProduct d s t) = QProduct (k * d) s t

>instance (LiteralUnit a, LiteralUnit b, Scalar a ~ Scalar b) => VectorSpace (a :/ b) where
>   type Scalar (a :/ b) = Scalar a
>   vzero = QDivide 0 fromAmount fromAmount
>   vnegate (QDivide x a b) = QDivide (negate x) a b
>   (QDivide x a b) %+ (QDivide x' _ _) = QDivide (x + x') a b
>   k %* (QDivide d s t) = QDivide (k * d) s t

>instance (LiteralUnit a, LiteralUnit b, Show (Scalar a), Scalar a ~ Scalar b) => Unit (a :* b) where
>   amount = qproduct_amount
>   unitOf z@(QProduct x _ _) = show (dimension z)
>   fromQuantity q = do
>     let res = QProduct (value_amount q) fromAmount fromAmount
>     guard (value_dimension q == dimension res)
>       <|> invalidDimensions "fromQuantity" (value_dimension q) (dimension res)
>                                           (value_amount q) (value_amount q)
>     return res
>   dimension (QProduct x s t) = unitNameToDimension s + unitNameToDimension t

>instance (Scalar a ~ Scalar b, Show (Scalar a), LiteralUnit a, LiteralUnit b) => LiteralUnit (a :* b) where
>   fromAmount d = QProduct d fromAmount fromAmount 

>instance (LiteralUnit a, LiteralUnit b, Show (Scalar a), Scalar a ~ Scalar b) => Unit (a :/ b) where
>   amount = qdivide_amount
>   unitOf z@(QDivide x _ _) = show (dimension z)
>   fromQuantity q = do
>      let res = QDivide (value_amount q) fromAmount fromAmount 
>      guard (value_dimension q == dimension res)
>        <|> invalidDimensions "fromQuantity" (value_dimension q) (dimension res)
>                                            (value_amount q) (value_amount q)
>      return $ res
>   dimension (QDivide x a b) = unitNameToDimension a - unitNameToDimension b

>instance (Scalar a ~ Scalar b, Show (Scalar a), LiteralUnit a, LiteralUnit b) => LiteralUnit (a :/ b) where
>   fromAmount d = QDivide d fromAmount fromAmount

>instance (Scalar a ~ Scalar b, LiteralUnit a, LiteralUnit b, Show (Scalar a)) => Show (a :* b) where
>   show z@(QProduct d s t) = show d ++ " " ++ show (dimension z)

>instance (Scalar a ~ Scalar b, LiteralUnit a, LiteralUnit b, Show (Scalar a)) => Show (a :/ b) where
>   show z@(QDivide d s t) = show d ++ " " ++ show (dimension z)
>    -- show (unitNameToDimension s)
>      --                             ++ "/(" ++ show (unitNameToDimension t) ++ ")"

>instance Unit Float where
>   amount x = x
>   unitOf _ = ""
>   fromQuantity (x `As` d) = guard (isDimensionless d) >> return x
>   dimension _ = dimensionless

>instance LiteralUnit Float where
>   fromAmount x = x

>instance Unit Double where
>   amount x = x
>   unitOf _ = ""
>   dimension _ = dimensionless
>   fromQuantity (x `As` d) = guard (isDimensionless d) >> return x

>instance LiteralUnit Double where
>   fromAmount x = x

>instance Unit R where
>   amount x = x
>   unitOf _ = ""
>   dimension _ = dimensionless
>   fromQuantity (x `As` d) = guard (isDimensionless d) >> return x

>instance LiteralUnit R where
>   fromAmount x = x

>newtype Dimensionless = Dimensionless { dimensionless_value :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary, Num, Fractional, Real, RealFrac, Floating, RealFloat)
>instance Show Dimensionless where { show = show_unit }
>instance Read Dimensionless where
>   readPrec = readPrec >>= (return . Dimensionless)
>instance VectorSpace Dimensionless where
>   type Scalar Dimensionless = Double
>   vzero = Dimensionless 0
>   vnegate (Dimensionless x) = Dimensionless (negate x)
>   (Dimensionless x) %+ (Dimensionless y) = Dimensionless $ x + y
>   k %* (Dimensionless x) = Dimensionless $ k * x


>instance NormedSpace Dimensionless where
>   norm = amount

>instance Unit Dimensionless where
>   amount = dimensionless_value
>   unitOf _ = ""
>   dimension _ = dimensionless
>   fromQuantity (x `As` d) = do
>     guard (isDimensionless d)
>     return $ Dimensionless x

>instance LiteralUnit Dimensionless where
>   fromAmount = Dimensionless

>newtype Information = Bits { number_of_bits :: Double }
>  deriving (Eq,Ord, Data,Generic, Typeable)
>  deriving newtype (Binary)
>instance Show Information where { show = show_unit }

>instance Unit Information where
>   amount = number_of_bits
>   dimension _ = dimensionless
>   fromQuantity = fromQuantityDef dimensionless Bits
>   unitOf _ = "b"
>instance LiteralUnit Information where { fromAmount = Bits }
> 
>newtype SoundLevel = SoundAmplitude { sound_amplitude :: Double }
>  deriving (Eq, Ord, Typeable, Data, Generic)
>  deriving newtype (Binary)

>instance Show SoundLevel where { show = show_unit }
>-- | NOTE: additive operations mapped to multiplicative.
>-- Notice this reduces the possible range of SoundLevel values
>-- to around -300..300 dB based on possible exponents in Double.
>instance VectorSpace SoundLevel where
>   type Scalar SoundLevel = Double
>   vzero = SoundAmplitude 1
>   vnegate (SoundAmplitude x) = SoundAmplitude (1/x)
>   (SoundAmplitude x) %+ (SoundAmplitude y) = SoundAmplitude (x * y)
>   k %* (SoundAmplitude x) = SoundAmplitude (x ** k)

>instance Unit SoundLevel where
>   amount x = log (sound_amplitude x) / log 10
>   dimension _ = dimensionless
>   fromQuantity = fromQuantityDef dimensionless fromAmount
>   unitOf _ = "dB"
>instance LiteralUnit SoundLevel where { fromAmount = SoundAmplitude . (10.0 **) }


>newtype Angle = Radians { radians :: Double }
>   deriving (Eq,Ord, Typeable, Data, Generic)
>   deriving newtype (Binary, Num, Fractional)

>instance Show Angle where { show = show_unit }

>pi_angle :: Angle
>pi_angle = Radians pi

>fraction_of_circle :: Rational -> Angle
>fraction_of_circle r = fromRational (2*r) %* pi_angle

>asin_angle :: Double -> Angle
>asin_angle = Radians . asin

>acos_angle :: Double -> Angle
>acos_angle = Radians . acos
>
>atan_angle :: Double -> Angle
>atan_angle = Radians . atan

>sin_angle :: Angle -> Double
>sin_angle = sin . radians

>cos_angle :: Angle -> Double
>cos_angle = cos . radians
>
>tan_angle :: Angle -> Double
>tan_angle = tan . radians

>log_angle :: Complex Double -> Angle
>log_angle = Radians . phase

>exp_angle :: Angle -> Complex Double
>exp_angle (Radians x) = (cos x :+ sin x)
>
>fromPolar :: (Unit u, Scalar u ~ Double) => u -> Angle -> Complex u
>fromPolar r (Radians alfa) = (cos alfa %* r :+ sin alfa %* r)

>toPolar :: (LiteralUnit u, Scalar u ~ Double) => Complex u -> (u, Angle)
>toPolar z@((a :: u) :+ b) = (fromAmount (sqrt (a'*a' %+ b'*b')) :: u , Radians $ atan2 b' a')
>   where a' = amount a
>         b' = amount b

>newtype DegreesAngle = Degrees { degrees :: Double }
>   deriving (Eq,Ord, Typeable, Data, Generic)
>   deriving newtype (Binary)


>instance Show DegreesAngle where { show = show_unit }


>degreesAngle :: DegreesAngle -> Angle
>degreesAngle (Degrees d) = Radians $ (d * pi) / 180.0

>angleDegrees :: Angle -> DegreesAngle
>angleDegrees (Radians r) = Degrees $ r * 180.0 / pi

>isoDegreesToAngle :: DegreesAngle :==: Angle
>isoDegreesToAngle = degreesAngle <-> angleDegrees

>instance Unit DegreesAngle where
>   amount = degrees
>   dimension _ = dimensionless
>   fromQuantity = fromQuantityDef dimensionless Degrees
>   unitOf _ = "°"
>instance LiteralUnit DegreesAngle where
>   fromAmount = Degrees
>   zeroAmount _ = 0
>   conversionFactor _ = pi/180.0


>instance Unit Angle where
>   amount = radians
>   dimension _ = dimensionless
>   fromQuantity = fromQuantityDef dimensionless Radians
>   unitOf _ = "rad"
>instance LiteralUnit Angle where { fromAmount = Radians }

>-- | <https://en.wikipedia.org/wiki/Steradian>
>newtype SolidAngle = Steradians { steradians :: Double }
>   deriving (Eq,Ord, Typeable, Data, Generic)
>   deriving newtype (Binary)

>instance Show SolidAngle where { show = show_unit }

>instance Unit SolidAngle where
>   amount = steradians
>   dimension _ = dimensionless
>   fromQuantity = fromQuantityDef dimensionless Steradians
>   unitOf _ = "sr"
>instance LiteralUnit SolidAngle where { fromAmount = Steradians }

>show_unit :: (Unit u, Show (Scalar u)) => u -> String
>show_unit i = show (amount i) ++ " " ++ unitOf i

>read_unit :: (Unit u, Read (Scalar u)) => UnitName u -> Text.ParserCombinators.ReadPrec.ReadPrec u
>read_unit unitname = readPrec >>= \d -> lift skipSpaces
> >> lift (string $ unitOf $ unitname d)
> >> return (unitname d)

>newtype Percentage = Percentages { percentages :: Double }
>  deriving (Eq,Ord, Typeable, Data, Generic)
>  deriving newtype (Binary)

>instance Show Percentage where { show = show_unit }
>
>instance Unit Percentage where
>  amount = percentages
>  fromQuantity = fromQuantityDef dimensionless (Percentages . (100.0*))
>  dimension _ = dimensionless
>  unitOf _ = "%"
>instance LiteralUnit Percentage where
>   fromAmount = Percentages
>   conversionFactor _ = 0.01
>
>newtype Acceleration = MetersPerSquareSecond { metersPerSquareSecond :: Double }
>   deriving (Eq,Ord, Typeable, Data, Generic)
>   deriving newtype (Binary)

>instance Show Acceleration where { show = show_unit }

>instance Unit Acceleration where
>   amount = metersPerSquareSecond
>   fromQuantity = fromQuantityDef (meter_dimension %- (second_dimension %+ second_dimension)) MetersPerSquareSecond
>   dimension _ = meter_dimension %- (second_dimension %+ second_dimension)
>   unitOf _ = "m/s^2"
>instance LiteralUnit Acceleration where { fromAmount = MetersPerSquareSecond }
>newtype Velocity = MetersPerSecond { metersPerSecond :: Double }
>   deriving (Eq,Ord, Typeable, Data, Generic)
>   deriving newtype (Binary)
>instance Show Velocity where { show = show_unit }
>
>instance Unit Velocity where
>   amount = metersPerSecond
>   fromQuantity = fromQuantityDef (meter_dimension %- second_dimension) MetersPerSecond
>   dimension _ = meter_dimension %- second_dimension
>   unitOf _ = "m/s"
>instance LiteralUnit Velocity where { fromAmount = MetersPerSecond }
>newtype SquareLength = SquareMeters { squaremeters :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)

>newtype CubicLength = CubicMeters { cubicmeters :: Double }
>  deriving (Eq,Ord, Typeable, Data, Generic)
>  deriving newtype (Binary)
>
>sqrt_length :: SquareLength -> Length
>sqrt_length (SquareMeters m) = Meters (sqrt m)

>square_length :: Length -> SquareLength
>square_length (Meters m) = SquareMeters (m * m)

>times_length :: Length -> Length -> SquareLength
>times_length (Meters m1) (Meters m2) = SquareMeters (m1 * m2)

>instance Show CubicLength where { show = show_unit }
>instance Unit CubicLength where
>   amount = cubicmeters
>   fromQuantity = fromQuantityDef cubicmeter_dimension CubicMeters
>   dimension _ = 3 %* meter_dimension
>   unitOf _ = "m^3"
>instance LiteralUnit CubicLength where { fromAmount = CubicMeters }
>instance Show SquareLength where { show = show_unit }
>instance Unit SquareLength where
>   amount = squaremeters
>   fromQuantity = fromQuantityDef squaremeter_dimension SquareMeters
>   dimension _ = meter_dimension %+ meter_dimension
>   unitOf _ = "m^2"
>instance LiteralUnit SquareLength where { fromAmount = SquareMeters }
>-- types for basic units <https://en.wikipedia.org/wiki/International_System_of_Units>
>newtype Length = Meters { meters :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Length where { show = show_unit }

>newtype Mass = Kilograms { kilograms :: Double }
>  deriving (Eq,Ord, Typeable, Data, Generic)
>  deriving newtype (Binary)
>instance Show Mass where { show = show_unit }
>newtype Time = Seconds { seconds :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Time where { show = show_unit }

>newtype Current = Amperes { amperes :: Double }
>  deriving (Eq,Ord, Typeable, Data, Generic)
>  deriving newtype (Binary)
>instance Show Current where { show = show_unit }

>-- | <https://en.wikipedia.org/wiki/Fahrenheit>
>newtype DegreesFahrenheit = DegreesFahrenheit { fahrenheits :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show DegreesFahrenheit where { show = show_unit }
>instance Unit DegreesFahrenheit where
>   amount = fahrenheits
>   dimension _ = kelvin_dimension
>   unitOf _ = "°F"
>   fromQuantity x
>     | dimension x == kelvin_dimension = return $ DegreesFahrenheit $
>                            (amount x * (9/5) - 459.67)
>     | otherwise = invalidDimensions "fromQuantity:DegreesFahrenheit" (dimension x) kelvin_dimension (amount x) (amount x)
>instance LiteralUnit DegreesFahrenheit where
>  fromAmount = DegreesFahrenheit
>  conversionFactor _ = 5/9
>  zeroAmount _ = 459.67
> 
>newtype DegreesTemperature = DegreesCelcius { celciuses :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show DegreesTemperature where { show = show_unit }
>
>instance Unit DegreesTemperature where
>   amount = celciuses
>   dimension _ = kelvin_dimension
>   unitOf _ = "°C"
>   fromQuantity x
>     | dimension x == kelvin_dimension = return $ DegreesCelcius $ amount (x - (273.15 @@ kelvin_dimension))
>     | otherwise = invalidDimensions "fromQuantity:DegreesTemperature" (dimension x) kelvin_dimension (amount x) (amount x)

>instance LiteralUnit DegreesTemperature where
>   fromAmount = DegreesCelcius
>   zeroAmount _ = 273.15

>newtype Temperature = DegreesKelvin { kelvins :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Temperature where { show = show_unit }

>isoCelciusToKelvin :: DegreesTemperature :==: Temperature
>isoCelciusToKelvin = celciusToKelvin <-> kelvinToCelcius
>
>celciusToKelvin :: DegreesTemperature -> Temperature
>celciusToKelvin (DegreesCelcius x) = DegreesKelvin (x + 273.15)

>kelvinToCelcius :: Temperature -> DegreesTemperature
>kelvinToCelcius (DegreesKelvin x) = DegreesCelcius (x - 273.15)

>newtype Substance = Moles { moles :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Substance where { show = show_unit }

>newtype Intensity = Candelas { candelas :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Intensity where { show = show_unit }

>newtype Frequency = Hertzes { hertzes :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Frequency where { show = show_unit }

>newtype Force = Newtons { newtons :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Force where { show = show_unit }

>newtype Torque = NewtonMeters { newtonmeters :: Double }
>  deriving (Eq, Ord, Typeable, Data, Generic)
>  deriving newtype (Binary)
>instance Show Torque where { show = show_unit }

>newtype Pressure = Pascals { pascals :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Pressure where { show = show_unit }

>newtype Energy = Joules { joules :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Energy where { show = show_unit }

>newtype Power = Watts { watts :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Power where { show = show_unit }

>newtype Charge = Coulombs { coulombs :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Charge where { show = show_unit }

>newtype Voltage = Volts { volts :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Voltage where { show = show_unit }

>newtype Capacitance = Farads { farads :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic) deriving newtype (Binary)
>instance Show Capacitance where { show = show_unit }

>newtype Resistance = Ohms { ohms :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Resistance where { show = show_unit }

>newtype Conductance = Siemenses { siemenses :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic) deriving newtype (Binary)
>instance Show Conductance where { show = show_unit }

>newtype Flux = Webers { webers :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic) deriving newtype (Binary)
>instance Show Flux where { show = show_unit }

>newtype FluxDensity = Teslas { teslas :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic) deriving newtype (Binary)
>instance Show FluxDensity where { show = show_unit }

>newtype Inductance  = Henrys { henrys :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Inductance where { show = show_unit }

>newtype LuminousFlux = Lumens { lumens :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic) deriving newtype (Binary)
>instance Show LuminousFlux where { show = show_unit }

>newtype Illuminance = Luxes { luxes :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Illuminance where { show = show_unit }

>newtype Radioactivity = Becquerels { becquerels :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Radioactivity where { show = show_unit }

>newtype AbsorbedDose  = Grays { grays :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show AbsorbedDose where { show = show_unit }

>newtype EquivalentDose = Sieverts { sieverts :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)

>instance Show EquivalentDose where { show = show_unit }


>newtype CatalyticActivity = Katals { katals :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show CatalyticActivity where { show = show_unit }

>instance Unit CatalyticActivity where
>   amount = katals
>   dimension _ = katal_dimension
>   fromQuantity = fromQuantityDef katal_dimension Katals
>   unitOf _ = "kat"
>instance LiteralUnit CatalyticActivity where { fromAmount = Katals }
>instance Unit EquivalentDose where
>   amount = sieverts
>   dimension _ = sievert_dimension
>   fromQuantity = fromQuantityDef sievert_dimension Sieverts
>   unitOf _ = "Sv"
>instance LiteralUnit EquivalentDose where { fromAmount = Sieverts }
>   
>                                     
>instance Unit AbsorbedDose where
>   amount = grays
>   dimension _ = gray_dimension
>   fromQuantity = fromQuantityDef gray_dimension Grays
>   unitOf _ = "Gy"
>instance LiteralUnit AbsorbedDose where { fromAmount = Grays }   
>instance Unit Radioactivity where
>   amount = becquerels
>   dimension _ = becquerel_dimension
>   fromQuantity = fromQuantityDef becquerel_dimension Becquerels
>   unitOf _ = "Bq"
>instance LiteralUnit Radioactivity where { fromAmount = Becquerels }
>instance Unit Illuminance where
>   amount = luxes
>   dimension _ = lux_dimension
>   fromQuantity = fromQuantityDef lux_dimension Luxes
>   unitOf _ = "lx"
>instance LiteralUnit Illuminance where { fromAmount = Luxes }
>instance Unit LuminousFlux where
>   amount = lumens
>   dimension _ = lumen_dimension
>   fromQuantity = fromQuantityDef lumen_dimension Lumens
>   unitOf _ = "lm"
>instance LiteralUnit LuminousFlux where { fromAmount = Lumens }
>instance Unit Inductance where
>   amount = henrys
>   dimension _ = henry_dimension
>   fromQuantity = fromQuantityDef henry_dimension Henrys
>   unitOf _ = "H"
>instance LiteralUnit Inductance where { fromAmount = Henrys }
>instance Unit FluxDensity where
>   amount = teslas
>   dimension _ = tesla_dimension
>   fromQuantity = fromQuantityDef tesla_dimension Teslas
>   unitOf _ = "T"
>instance LiteralUnit FluxDensity where { fromAmount = Teslas }
>instance Unit Flux where
>   amount = webers
>   dimension _ = weber_dimension
>   fromQuantity = fromQuantityDef weber_dimension Webers
>   unitOf _ = "W"
>instance LiteralUnit Flux where { fromAmount = Webers }
>instance Unit Conductance where
>   amount = siemenses
>   dimension _ = siemens_dimension
>   fromQuantity = fromQuantityDef siemens_dimension Siemenses
>   unitOf _ = "S"
>instance LiteralUnit Conductance where { fromAmount = Siemenses }
>instance Unit Resistance where
>   amount = ohms
>   dimension _ = ohm_dimension
>   fromQuantity = fromQuantityDef ohm_dimension Ohms
>   unitOf _ = "Ω"
>instance LiteralUnit Resistance where { fromAmount = Ohms }
>instance Unit Capacitance where
>   amount = farads
>   dimension _ = farad_dimension
>   fromQuantity = fromQuantityDef farad_dimension Farads
>   unitOf _ = "F"
>instance LiteralUnit Capacitance where { fromAmount = Farads }
>instance Unit Voltage where
>   amount = volts
>   dimension _ = volt_dimension
>   fromQuantity = fromQuantityDef volt_dimension Volts
>   unitOf _ = "V"
>instance LiteralUnit Voltage where { fromAmount = Volts }
>instance Unit Charge where
>   amount = coulombs
>   dimension _ = coulomb_dimension
>   fromQuantity = fromQuantityDef coulomb_dimension Coulombs
>   unitOf _ = "C"
>instance LiteralUnit Charge where { fromAmount = Coulombs }
>instance Unit Power where
>   amount = watts
>   dimension _ = watt_dimension
>   fromQuantity = fromQuantityDef watt_dimension Watts
>   unitOf _ = "W"
>instance LiteralUnit Power where { fromAmount = Watts }
>instance Unit Energy where
>   amount = joules
>   dimension _ = joule_dimension
>   fromQuantity = fromQuantityDef joule_dimension Joules
>   unitOf _ = "J"
>instance LiteralUnit Energy where { fromAmount = Joules }
>instance Unit Pressure where
>   amount = pascals
>   dimension _ = pascal_dimension
>   fromQuantity = fromQuantityDef pascal_dimension Pascals
>   unitOf _ = "Pa"
>instance LiteralUnit Pressure where { fromAmount = Pascals }
>instance Unit Force where
>   amount = newtons
>   dimension _ = newton_dimension
>   fromQuantity = fromQuantityDef newton_dimension Newtons
>   unitOf _ = "N"
>instance LiteralUnit Force where { fromAmount = Newtons }
>instance Unit Torque where
>   amount = newtonmeters
>   dimension _ = joule_dimension %- radian_dimension
>   fromQuantity = fromQuantityDef (joule_dimension %- radian_dimension) NewtonMeters
>   unitOf _ = "N m" -- notice not displayed similarly than joule.
>instance LiteralUnit Torque where { fromAmount = NewtonMeters }

>instance Unit Frequency where
>   amount = hertzes
>   dimension _ = hertz_dimension
>   fromQuantity = fromQuantityDef hertz_dimension Hertzes
>   unitOf _ = "Hz"
>instance LiteralUnit Frequency where { fromAmount = Hertzes }
>instance Unit Length where
>   amount = meters
>   dimension _ = meter_dimension
>   fromQuantity = fromQuantityDef meter_dimension Meters
>   unitOf _ = "m"
>instance LiteralUnit Length where { fromAmount = Meters }
>instance Unit Mass where
>   amount = kilograms
>   dimension _ = kilogram_dimension
>   fromQuantity = fromQuantityDef kilogram_dimension Kilograms
>   unitOf _ = "kg"
>instance LiteralUnit Mass where { fromAmount = Kilograms }
>instance Unit Time where
>   amount = seconds
>   dimension _ = second_dimension
>   fromQuantity = fromQuantityDef second_dimension Seconds
>   unitOf _ = "s"
>instance LiteralUnit Time where { fromAmount = Seconds }
>instance Unit Current where
>   amount = amperes
>   dimension _ = ampere_dimension
>   fromQuantity = fromQuantityDef ampere_dimension Amperes
>   unitOf _ = "A"
>instance LiteralUnit Current where { fromAmount = Amperes }
>instance Unit Temperature where
>   amount = kelvins
>   dimension _ = kelvin_dimension
>   fromQuantity = fromQuantityDef kelvin_dimension DegreesKelvin
>   unitOf _ = "K"
>instance LiteralUnit Temperature where { fromAmount = DegreesKelvin }
> 
>instance Unit Substance where
>   amount = moles
>   dimension _ = mol_dimension
>   fromQuantity = fromQuantityDef mol_dimension Moles
>   unitOf _ = "mol"
>instance LiteralUnit Substance where { fromAmount = Moles }
>instance Unit Intensity where
>   amount = candelas
>   dimension _ = candela_dimension
>   fromQuantity = fromQuantityDef candela_dimension Candelas
>   unitOf _ = "cd"
>instance LiteralUnit Intensity where { fromAmount = Candelas }
>instance Read Angle where { readPrec = read_unit fromAmount }
>instance Read Resistance where { readPrec = read_unit fromAmount }
>instance Read DegreesAngle where { readPrec = read_unit fromAmount }
>instance Read Information where { readPrec = read_unit fromAmount }
>instance Read Inductance where { readPrec = read_unit fromAmount }
>instance Read EquivalentDose where { readPrec = read_unit fromAmount }
>instance Read Conductance where { readPrec = read_unit fromAmount }
>instance Read CatalyticActivity where { readPrec = read_unit fromAmount }
>instance Read Radioactivity where { readPrec = read_unit fromAmount }
>instance Read Illuminance where { readPrec = read_unit fromAmount }
>instance Read AbsorbedDose where { readPrec = read_unit fromAmount }
>instance Read FluxDensity where { readPrec = read_unit fromAmount }
>instance Read Flux where { readPrec = read_unit fromAmount }
>instance Read LuminousFlux where { readPrec = read_unit fromAmount }
>instance Read Capacitance where { readPrec = read_unit fromAmount }
>instance Read Charge where { readPrec = read_unit fromAmount }
>instance Read Power where { readPrec = read_unit fromAmount }
>instance Read Voltage where { readPrec = read_unit fromAmount }
>instance Read Energy where { readPrec = read_unit fromAmount }
>instance Read Pressure where { readPrec = read_unit fromAmount }
>instance Read Force where { readPrec = read_unit fromAmount }
>instance Read Intensity where { readPrec = read_unit fromAmount }
>instance Read Substance where { readPrec = read_unit fromAmount }
>instance Read Temperature where { readPrec = read_unit fromAmount }
>instance Read Frequency where { readPrec = read_unit fromAmount }
>instance Read SolidAngle where { readPrec = read_unit fromAmount }
>instance Read DegreesTemperature where { readPrec = read_unit fromAmount }
>instance Read Current where { readPrec = read_unit fromAmount }
>instance Read DegreesFahrenheit where { readPrec = read_unit fromAmount }
>instance Read Percentage where { readPrec = read_unit fromAmount }
>instance Read Time where { readPrec = read_unit fromAmount }
>instance Read Length where { readPrec = read_unit fromAmount }
>instance Read Mass where { readPrec = read_unit fromAmount }
>instance Read SquareLength where { readPrec = read_unit fromAmount }
>instance Read CubicLength where { readPrec = read_unit fromAmount }
>instance Read Acceleration where { readPrec = read_unit fromAmount }
>instance Read Velocity where { readPrec = read_unit fromAmount }
>instance Read Torque where { readPrec = read_unit fromAmount }
>instance Read SoundLevel where { readPrec = read_unit fromAmount }
>instance (LiteralUnit a, LiteralUnit b, Read (Scalar a), Show (Scalar a), Scalar a ~ Scalar b) => Read (a :* b) where { readPrec = read_unit (fromAmount *%% fromAmount) }
>instance (LiteralUnit a, LiteralUnit b, Read (Scalar a), Show (Scalar a), Scalar a ~ Scalar b) => Read (a :/ b) where { readPrec = read_unit (fromAmount /%% fromAmount) }

#if __GLASGOW_HASKELL__ >= 806

>deriving via Dimensionless instance VectorSpace (Angle)
>deriving via Dimensionless instance VectorSpace (Resistance)
>deriving via Dimensionless instance VectorSpace (DegreesAngle)
>deriving via Dimensionless instance VectorSpace (Information)
>deriving via Dimensionless instance VectorSpace (Inductance)
>deriving via Dimensionless instance VectorSpace (EquivalentDose)
>deriving via Dimensionless instance VectorSpace (Conductance)
>deriving via Dimensionless instance VectorSpace (CatalyticActivity)
>deriving via Dimensionless instance VectorSpace (Radioactivity)
>deriving via Dimensionless instance VectorSpace (Illuminance)
>deriving via Dimensionless instance VectorSpace (AbsorbedDose)
>deriving via Dimensionless instance VectorSpace (FluxDensity)
>deriving via Dimensionless instance VectorSpace (Flux)
>deriving via Dimensionless instance VectorSpace (LuminousFlux)
>deriving via Dimensionless instance VectorSpace (Capacitance)
>deriving via Dimensionless instance VectorSpace (Charge)
>deriving via Dimensionless instance VectorSpace (Power)
>deriving via Dimensionless instance VectorSpace (Voltage)
>deriving via Dimensionless instance VectorSpace (Energy)
>deriving via Dimensionless instance VectorSpace (Pressure)
>deriving via Dimensionless instance VectorSpace (Force)
>deriving via Dimensionless instance VectorSpace (Intensity)
>deriving via Dimensionless instance VectorSpace (Substance)
>deriving via Dimensionless instance VectorSpace (Temperature)
>deriving via Dimensionless instance VectorSpace (Frequency)
>deriving via Dimensionless instance VectorSpace (SolidAngle)
>deriving via Dimensionless instance VectorSpace (DegreesTemperature)
>deriving via Dimensionless instance VectorSpace (Current)
>deriving via Dimensionless instance VectorSpace (DegreesFahrenheit)
>deriving via Dimensionless instance VectorSpace (Percentage)
>deriving via Dimensionless instance VectorSpace (Time)
>deriving via Dimensionless instance VectorSpace (Length)
>deriving via Dimensionless instance VectorSpace (Mass)
>deriving via Dimensionless instance VectorSpace (SquareLength)
>deriving via Dimensionless instance VectorSpace (CubicLength)
>deriving via Dimensionless instance VectorSpace (Acceleration)
>deriving via Dimensionless instance VectorSpace (Velocity)
>deriving via Dimensionless instance VectorSpace (Torque)

>deriving via Dimensionless instance NormedSpace (Angle)
>deriving via Dimensionless instance NormedSpace (Resistance)
>deriving via Dimensionless instance NormedSpace (DegreesAngle)
>deriving via Dimensionless instance NormedSpace (Information)
>deriving via Dimensionless instance NormedSpace (Inductance)
>deriving via Dimensionless instance NormedSpace (EquivalentDose)
>deriving via Dimensionless instance NormedSpace (Conductance)
>deriving via Dimensionless instance NormedSpace (CatalyticActivity)
>deriving via Dimensionless instance NormedSpace (Radioactivity)
>deriving via Dimensionless instance NormedSpace (Illuminance)
>deriving via Dimensionless instance NormedSpace (AbsorbedDose)
>deriving via Dimensionless instance NormedSpace (FluxDensity)
>deriving via Dimensionless instance NormedSpace (Flux)
>deriving via Dimensionless instance NormedSpace (LuminousFlux)
>deriving via Dimensionless instance NormedSpace (Capacitance)
>deriving via Dimensionless instance NormedSpace (Charge)
>deriving via Dimensionless instance NormedSpace (Power)
>deriving via Dimensionless instance NormedSpace (Voltage)
>deriving via Dimensionless instance NormedSpace (Energy)
>deriving via Dimensionless instance NormedSpace (Pressure)
>deriving via Dimensionless instance NormedSpace (Force)
>deriving via Dimensionless instance NormedSpace (Intensity)
>deriving via Dimensionless instance NormedSpace (Substance)
>deriving via Dimensionless instance NormedSpace (Temperature)
>deriving via Dimensionless instance NormedSpace (Frequency)
>deriving via Dimensionless instance NormedSpace (SolidAngle)
>deriving via Dimensionless instance NormedSpace (DegreesTemperature)
>deriving via Dimensionless instance NormedSpace (Current)
>deriving via Dimensionless instance NormedSpace (DegreesFahrenheit)
>deriving via Dimensionless instance NormedSpace (Percentage)
>deriving via Dimensionless instance NormedSpace (Time)
>deriving via Dimensionless instance NormedSpace (Length)
>deriving via Dimensionless instance NormedSpace (Mass)
>deriving via Dimensionless instance NormedSpace (SquareLength)
>deriving via Dimensionless instance NormedSpace (Acceleration)
>deriving via Dimensionless instance NormedSpace (Velocity)
>deriving via Dimensionless instance NormedSpace (Torque)

#else

>instance VectorSpace Velocity where
>   type Scalar Velocity = Double
>   vzero = MetersPerSecond 0
>   vnegate (MetersPerSecond x) = MetersPerSecond (negate x)
>   (MetersPerSecond x) %+ (MetersPerSecond y) = MetersPerSecond $ x + y
>   k %* (MetersPerSecond x) = MetersPerSecond (k * x)
>instance NormedSpace Velocity where { norm = amount }
>instance VectorSpace Acceleration where
>   type Scalar Acceleration = Double
>   vzero = MetersPerSquareSecond 0
>   vnegate (MetersPerSquareSecond x) = MetersPerSquareSecond (negate x)
>   (MetersPerSquareSecond x) %+ (MetersPerSquareSecond y) = MetersPerSquareSecond (x + y)
>   k %* (MetersPerSquareSecond x) = MetersPerSquareSecond (k %* x)
>instance NormedSpace Acceleration where { norm = amount }
>instance VectorSpace CubicLength where
>   type Scalar CubicLength = Double
>   vzero = CubicMeters 0
>   vnegate (CubicMeters x) = CubicMeters $ negate x
>   (CubicMeters x) %+ (CubicMeters y) = CubicMeters $ x + y
>   k %* (CubicMeters x) = CubicMeters $ k * x
>instance VectorSpace SquareLength where
>   type Scalar SquareLength = Double
>   vzero = SquareMeters 0
>   vnegate (SquareMeters x) = SquareMeters $ negate x
>   (SquareMeters x) %+ (SquareMeters y) = SquareMeters $ x + y
>   k %* (SquareMeters x) = SquareMeters $ k * x
>instance NormedSpace SquareLength where { norm = amount }

>instance VectorSpace Mass where
>   type Scalar Mass = Double
>   vzero = Kilograms 0
>   vnegate (Kilograms x) = Kilograms $ negate x
>   (Kilograms x) %+ (Kilograms y) = Kilograms $ x + y
>   k %* (Kilograms x) = Kilograms $ k * x
>instance NormedSpace Mass where { norm = amount }
>instance VectorSpace Length where
>   type Scalar Length = Double
>   vzero = Meters 0
>   vnegate (Meters x) = Meters $ negate x
>   (Meters x) %+ (Meters y) = Meters $ x + y
>   k %* (Meters x) = Meters $ k * x
>instance NormedSpace Length where { norm = amount }
>instance VectorSpace Time where
>   type Scalar Time = Double
>   vzero = Seconds 0
>   vnegate (Seconds x) = Seconds $ negate x
>   (Seconds x) %+ (Seconds y) = Seconds $ x + y
>   k %* (Seconds x) = Seconds $ k * x
>instance NormedSpace Time where { norm = amount }
>instance VectorSpace Percentage where
>   type Scalar Percentage = Double
>   vzero = Percentages 0
>   vnegate (Percentages i) = Percentages $ negate i
>   (Percentages x) %+ (Percentages y) = Percentages $ x + y
>   k %* (Percentages x) = Percentages (k * x)
>instance NormedSpace Percentage where { norm = amount }
>instance VectorSpace DegreesFahrenheit where
>   type Scalar DegreesFahrenheit = Double
>   vzero = DegreesFahrenheit 0
>   vnegate (DegreesFahrenheit x) = DegreesFahrenheit $ negate x
>   (DegreesFahrenheit x) %+ (DegreesFahrenheit y) = DegreesFahrenheit $ x + y
>   k %* (DegreesFahrenheit x) = DegreesFahrenheit $ k * x
>instance NormedSpace DegreesFahrenheit where { norm = amount }
>instance VectorSpace Current where
>   type Scalar Current = Double
>   vzero = Amperes 0
>   vnegate (Amperes x) = Amperes $ negate x
>   (Amperes x) %+ (Amperes y) = Amperes $ x + y
>   k %* (Amperes x) = Amperes $ k * x
>instance NormedSpace Current where { norm = amount }
>instance VectorSpace DegreesTemperature where
>   type Scalar DegreesTemperature = Double
>   vzero = DegreesCelcius 0
>   vnegate (DegreesCelcius x) = DegreesCelcius $ negate x
>   (DegreesCelcius x) %+ (DegreesCelcius y) = DegreesCelcius $ x + y
>   k %* (DegreesCelcius x) = DegreesCelcius $ k * x
>instance NormedSpace DegreesTemperature where { norm = amount }
>instance VectorSpace SolidAngle where
>   type Scalar SolidAngle = Double
>   vzero = Steradians 0
>   vnegate (Steradians x) = Steradians (negate x)
>   (Steradians x) %+ (Steradians y) = Steradians $ x + y
>   k %* (Steradians x) = Steradians $ k * x
>instance NormedSpace SolidAngle where { norm = amount }
>instance VectorSpace Frequency where
>   type Scalar Frequency = Double
>   vzero = Hertzes 0
>   vnegate (Hertzes x) = Hertzes $ negate x
>   (Hertzes x) %+ (Hertzes y) = Hertzes $ x + y
>   k %* (Hertzes x) = Hertzes $ k * x
>instance NormedSpace Frequency where { norm = amount }
>instance VectorSpace Temperature where
>   type Scalar Temperature = Double
>   vzero = DegreesKelvin 0
>   vnegate (DegreesKelvin x) = DegreesKelvin $ negate x
>   (DegreesKelvin x) %+ (DegreesKelvin y) = DegreesKelvin $ x + y
>   k %* (DegreesKelvin x) = DegreesKelvin $ k * x
>instance NormedSpace Temperature where { norm = amount }
>instance VectorSpace Substance where
>   type Scalar Substance = Double
>   vzero = Moles 0
>   vnegate (Moles x) = Moles $ negate x
>   (Moles x) %+ (Moles y) = Moles $ x + y
>   k %* (Moles x) = Moles $ k * x
>instance NormedSpace Substance where { norm = amount }
>instance VectorSpace Intensity where
>   type Scalar Intensity = Double
>   vzero = Candelas 0
>   vnegate (Candelas x) = Candelas $ negate x
>   (Candelas x) %+ (Candelas y) = Candelas $ x + y
>   k %* (Candelas x) = Candelas $ k * x
>instance NormedSpace Intensity where { norm = amount }
>instance VectorSpace Force where
>   type Scalar Force = Double
>   vzero = Newtons 0
>   vnegate (Newtons x) = Newtons $ negate x
>   (Newtons x) %+ (Newtons y) = Newtons $ x + y
>   k %* (Newtons x) = Newtons $ k * x
>instance NormedSpace Force where { norm = amount }
>instance VectorSpace Pressure where
>   type Scalar Pressure = Double
>   vzero = Pascals 0
>   vnegate (Pascals x) = Pascals $ negate x
>   (Pascals x) %+ (Pascals y) = Pascals $ x + y
>   k %* (Pascals x) = Pascals $ k * x
>instance NormedSpace Pressure where { norm = amount }
>instance VectorSpace Energy where
>   type Scalar Energy = Double
>   vzero = Joules 0
>   vnegate (Joules x) = Joules $ negate x
>   (Joules x) %+ (Joules y) = Joules $ x + y
>   k %* (Joules x) = Joules $ k * x
>instance NormedSpace Energy where { norm = amount }
>instance VectorSpace Voltage where
>   type Scalar Voltage = Double
>   vzero = Volts 0
>   vnegate (Volts x) = Volts $ negate x
>   (Volts x) %+ (Volts y) = Volts $ x + y
>   k %* (Volts x) = Volts $ k * x
>instance NormedSpace Voltage where { norm = amount }
>instance VectorSpace Power where
>   type Scalar Power = Double
>   vzero = Watts 0
>   vnegate (Watts x) = Watts $ negate x
>   (Watts x) %+ (Watts y) = Watts $ x + y
>   k %* (Watts x) = Watts $ k * x
>instance NormedSpace Power where { norm = amount }
>instance VectorSpace Charge where
>   type Scalar Charge = Double
>   vzero = Coulombs 0
>   vnegate (Coulombs x) = Coulombs $ negate x
>   (Coulombs x) %+ (Coulombs y) = Coulombs $ x + y
>   k %* (Coulombs x) = Coulombs $ k * x
>instance NormedSpace Charge where { norm = amount }
>instance VectorSpace Capacitance where
>   type Scalar Capacitance = Double
>   vzero = Farads 0
>   vnegate (Farads x) = Farads $ negate x
>   (Farads x) %+ (Farads y) = Farads $ x + y
>   k %* (Farads x) = Farads $ k * x
>instance NormedSpace Capacitance where { norm = amount }
>instance VectorSpace LuminousFlux where
>   type Scalar LuminousFlux = Double
>   vzero = Lumens 0
>   vnegate (Lumens x) = Lumens $ negate x
>   (Lumens x) %+ (Lumens y) = Lumens $ x + y
>   k %* (Lumens x) = Lumens $ k * x
>instance NormedSpace LuminousFlux where { norm = amount }
>instance VectorSpace Flux where
>   type Scalar Flux = Double
>   vzero = Webers 0
>   vnegate (Webers x) = Webers $ negate x
>   (Webers x) %+ (Webers y) = Webers $ x + y
>   k %* (Webers x) = Webers $ k * x
>instance NormedSpace Flux where { norm = amount }

>instance VectorSpace FluxDensity where
>   type Scalar FluxDensity = Double
>   vzero = Teslas 0
>   vnegate (Teslas x) = Teslas $ negate x
>   (Teslas x) %+ (Teslas y) = Teslas $ x + y
>   k %* (Teslas x) = Teslas $ k * x
>instance NormedSpace FluxDensity where { norm = amount }
>instance VectorSpace AbsorbedDose where
>   type Scalar AbsorbedDose = Double
>   vzero = Grays 0
>   vnegate (Grays x) = Grays $ negate x
>   (Grays x) %+ (Grays y) = Grays $ x + y
>   k %* (Grays x) = Grays $ k * x
>instance NormedSpace AbsorbedDose where { norm = amount }
>instance VectorSpace Illuminance where
>   type Scalar Illuminance = Double
>   vzero = Luxes 0
>   vnegate (Luxes x) = Luxes $ negate x
>   (Luxes x) %+ (Luxes y) = Luxes $ x + y
>   k %* (Luxes x) = Luxes $ k * x
>instance NormedSpace Illuminance where { norm = amount }
>instance VectorSpace Radioactivity where
>   type Scalar Radioactivity = Double
>   vzero = Becquerels 0
>   vnegate (Becquerels x) = Becquerels $ negate x
>   (Becquerels x) %+ (Becquerels y) = Becquerels $ x + y
>   k %* (Becquerels x) = Becquerels $ k * x
>instance NormedSpace Radioactivity where { norm = amount }
>instance VectorSpace CatalyticActivity where
>   type Scalar CatalyticActivity = Double
>   vzero = Katals 0
>   vnegate (Katals x) = Katals $ negate x
>   (Katals x) %+ (Katals y) = Katals $ x + y
>   k %* (Katals x) = Katals $ k * x
>instance NormedSpace CatalyticActivity where { norm = amount }
>instance VectorSpace Conductance where
>   type Scalar Conductance = Double
>   vzero = Siemenses 0
>   vnegate (Siemenses x) = Siemenses $ negate x
>   (Siemenses x) %+ (Siemenses y) = Siemenses $ x + y
>   k %* (Siemenses x) = Siemenses $ k * x
>instance NormedSpace Conductance where { norm = amount }

>instance VectorSpace EquivalentDose where
>   type Scalar EquivalentDose = Double
>   vzero = Sieverts 0
>   vnegate (Sieverts x) = Sieverts $ negate x
>   (Sieverts x) %+ (Sieverts y) = Sieverts $ x + y
>   k %* (Sieverts x) = Sieverts $ k * x
>instance NormedSpace EquivalentDose where { norm = amount }

>instance VectorSpace Angle where
>   type Scalar Angle = Double
>   vzero = Radians 0
>   vnegate (Radians x) = Radians (negate x)
>   (Radians x) %+ (Radians y) = Radians $ x + y
>   k %* (Radians x) = Radians $ k * x
>instance NormedSpace Angle where { norm = amount }

>instance VectorSpace Resistance where
>   type Scalar Resistance = Double
>   vzero = Ohms 0
>   vnegate (Ohms x) = Ohms $ negate x
>   (Ohms x) %+ (Ohms y) = Ohms $ x + y
>   k %* (Ohms x) = Ohms $ k * x
>instance NormedSpace Resistance where { norm = amount }

>instance VectorSpace DegreesAngle where
>   type Scalar DegreesAngle = Double
>   vzero = Degrees 0
>   vnegate (Degrees x) = Degrees $ negate x
>   (Degrees x) %+ (Degrees y) = Degrees $ x + y
>   k %* (Degrees x) = Degrees $ k * x
>instance NormedSpace DegreesAngle where { norm = amount }

>instance VectorSpace Information where
>   type Scalar Information = Double
>   vzero = Bits 0
>   vnegate (Bits x) = Bits (negate x)
>   (Bits x) %+ (Bits y) = Bits $ x + y
>   k %* (Bits x) = Bits (k * x)
>instance NormedSpace Information where { norm = amount }

>instance VectorSpace Inductance where
>   type Scalar Inductance = Double
>   vzero = Henrys 0
>   vnegate (Henrys x) = Henrys $ negate x
>   (Henrys x) %+ (Henrys y) = Henrys $ x + y
>   k %* (Henrys x) = Henrys $ k * x
>instance NormedSpace Inductance where { norm = amount }

>instance VectorSpace Torque where
>   type Scalar Torque = Double
>   vzero = NewtonMeters 0
>   vnegate (NewtonMeters x) = NewtonMeters $ negate x
>   (NewtonMeters x) %+ (NewtonMeters y) = NewtonMeters $ x + y
>   k %* (NewtonMeters x) = NewtonMeters $ k * x
>instance NormedSpace Torque where { norm = amount }

#endif
