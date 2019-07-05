>-- -*- coding: utf-8 -*-
>{-# LANGUAGE Trustworthy, CPP, TypeOperators #-}
>{-# LANGUAGE FlexibleInstances, TypeFamilyDependencies #-}
>{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
>{-# LANGUAGE ExistentialQuantification, TypeFamilies,GADTs, RankNTypes, UnicodeSyntax #-}
>{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, FlexibleContexts, DeriveGeneric, DeriveDataTypeable #-}
>-- | This module contains auxiliary definitions related to dimensional analysis.
>--   This is based on the SI system of units.
>--   This module supports compile-time checking for dimensional units.
>--
>--   It is not necessarily a good idea to make the checking in compile-time.
>--   In particular, this can prevent operations to be available, e.g. Num and Read
>--   classes are not supported for compile-time checked quantities. Num because it has
>--   wrong type for the multiplication and division. Instead of Num, VectorSpace operations
>--   and operations defined in this module should be used.
>--
>--   However, these types can distinguish quantities even in cases where the dimensions
>--   match, e.g. radians and steradians. Explicit conversions are nonetheless available
>--   when dimensions match.
>--
>--   However, it's very much likely that compile-time checked newtypes
>--   are faster at run-time than using the run-time checked quantities. This is probably
>--   the primary reason for wanting this module instead of run-time checked version.
>-- 
>--   Also it's not possible to cast input data directly without run-time check
>--   to a compile-time checked quantity.
>--
>--   Notice: Read instances exists only for run-time checked quantities.
>--
>--   See "Barton&Nackman: Scientific and Engineering C++" for C++ approach
>--   to dimensional analysis.
>--  
>--   <https://en.wikipedia.org/wiki/International_System_of_Units>
>module Math.Number.Units where
>import Control.Monad (guard)
>import Control.Applicative
>import Data.Typeable
>import Data.Data
>import GHC.Generics hiding (R)
>import Data.Binary
>import Data.Ratio
>import Data.Complex
>import Math.Matrix.Interface
>import Math.Number.DimensionalAnalysis
>import Math.Number.TypeRational
>import Data.Ratio
>import Math.Number.Real (R)

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
>(*%) :: (Unit u, Unit w, Num (Scalar u), Scalar u ~ Scalar w)
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
>(/%) :: (Fractional (Scalar u), Unit u, Unit w, Scalar u ~ Scalar w)
>     => u -> w -> u :/ w
>a /% b = QDivide (amount a / amount b) fromAmount fromAmount

>unitNameToDimension :: (Num (Scalar u), Unit u) => UnitName u -> Dimension
>unitNameToDimension f = dimension (f 0)

>mapAmount :: (Unit a) => (Scalar a -> Scalar a) -> a -> a
>mapAmount f = fromAmount . f . amount

>mapAmount2 :: (Unit a) => (Scalar a -> Scalar a -> Scalar a) -> a -> a -> a
>mapAmount2 f x y = fromAmount (f (amount x) (amount y))
>

>-- | Conversion from "Quantity a" to a unit-specific type.
>-- The second argument is the constructor for the newtype specific to the unit.
>-- 
>-- Example:  @(3 %* meter) `asUnit` Meter == return (Meter 3)@
>--           @0.3 `asUnit` Radians == return (Radians 0.3)@
>--  
>-- If the dimensions don't match, this raises an exception.
>asUnit :: (Monad m, Show (Scalar u), Show u, Unit u, Fractional (Scalar u))
>       => Quantity (Scalar u) -> UnitName u -> m u
>asUnit (x `As` d) f = let v = f (x / conversionFactor f - zeroAmount f)
> in if d == dimension v then return v
>                        else invalidDimensionsM "toUnit" d (dimension v) x (amount v)

>-- | Converts a compile-time checked dimensional unit to run-time checked version
>-- This often has the effect of reducing the complexity in types.
>quantity :: (Unit u) => u -> Quantity (Scalar u)
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
>instance (Unit a, Unit b, Scalar a ~ Scalar b) => VectorSpace (a :* b) where
>   type Scalar (a :* b) = Scalar a
>   vzero = QProduct 0 fromAmount fromAmount
>   vnegate (QProduct x s t) = QProduct (negate x) s t
>   (QProduct d s t) %+ (QProduct d' _ _) = QProduct (d + d') s t
>   k %* (QProduct d s t) = QProduct (k * d) s t

>instance (Unit a, Unit b, Scalar a ~ Scalar b) => VectorSpace (a :/ b) where
>   type Scalar (a :/ b) = Scalar a
>   vzero = QDivide 0 fromAmount fromAmount
>   vnegate (QDivide x a b) = QDivide (negate x) a b
>   (QDivide x a b) %+ (QDivide x' _ _) = QDivide (x + x') a b
>   k %* (QDivide d s t) = QDivide (k * d) s t

>instance (Unit a, Unit b, Show (Scalar a), Scalar a ~ Scalar b) => Unit (a :* b) where
>   amount = qproduct_amount
>   unitOf z@(QProduct x _ _) = show (dimension z)
>   fromAmount d = QProduct d fromAmount fromAmount
>   fromQuantity q = do
>     let res = QProduct (value_amount q) fromAmount fromAmount
>     guard (value_dimension q == dimension res)
>       <|> invalidDimensions "fromQuantity" (value_dimension q) (dimension res)
>                                           (value_amount q) (value_amount q)
>     return res
>   dimension (QProduct x s t) = unitNameToDimension s + unitNameToDimension t

>instance (Unit a, Unit b, Show (Scalar a), Scalar a ~ Scalar b) => Unit (a :/ b) where
>   amount = qdivide_amount
>   unitOf z@(QDivide x _ _) = show (dimension z)
>   fromAmount d = QDivide d fromAmount fromAmount
>   fromQuantity q = do
>      let res = QDivide (value_amount q) fromAmount fromAmount 
>      guard (value_dimension q == dimension res)
>        <|> invalidDimensions "fromQuantity" (value_dimension q) (dimension res)
>                                            (value_amount q) (value_amount q)
>      return $ res
>   dimension (QDivide x a b) = unitNameToDimension a - unitNameToDimension b

>instance (Scalar a ~ Scalar b, Unit a, Unit b, Show (Scalar a)) => Show (a :* b) where
>   show z@(QProduct d s t) = show d ++ " " ++ show (dimension z)

>instance (Scalar a ~ Scalar b, Unit a, Unit b, Show (Scalar a)) => Show (a :/ b) where
>   show z@(QDivide d s t) = show d ++ " " ++ show (dimension z)
>    -- show (unitNameToDimension s)
>      --                             ++ "/(" ++ show (unitNameToDimension t) ++ ")"

>instance Unit Float where
>   amount x = x
>   unitOf _ = ""
>   fromQuantity (x `As` d) = guard (isDimensionless d) >> return x
>   dimension _ = dimensionless
>   fromAmount x = x

>instance Unit Double where
>   amount x = x
>   unitOf _ = ""
>   dimension _ = dimensionless
>   fromQuantity (x `As` d) = guard (isDimensionless d) >> return x
>   fromAmount x = x

>instance Unit R where
>   amount x = x
>   unitOf _ = ""
>   dimension _ = dimensionless
>   fromQuantity (x `As` d) = guard (isDimensionless d) >> return x
>   fromAmount x = x

>newtype Dimensionless = Dimensionless { dimensionless_value :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary, Num, Fractional, Real, RealFrac, Floating, RealFloat)
>instance Show Dimensionless where { show = show_unit }
>instance VectorSpace Dimensionless where
>   type Scalar Dimensionless = Double
>   vzero = Dimensionless 0
>   vnegate (Dimensionless x) = Dimensionless (negate x)
>   (Dimensionless x) %+ (Dimensionless y) = Dimensionless $ x + y
>   k %* (Dimensionless x) = Dimensionless $ k * x

>instance Unit Dimensionless where
>   amount = dimensionless_value
>   unitOf _ = ""
>   fromAmount = Dimensionless
>   dimension _ = dimensionless
>   fromQuantity (x `As` d) = do
>     guard (isDimensionless d)
>     return $ Dimensionless x

>newtype Information = Bits { number_of_bits :: Double }
>  deriving (Eq,Ord, Data,Generic, Typeable)
>  deriving newtype (Binary)
>instance Show Information where { show = show_unit }
>instance VectorSpace Information where
>   type Scalar Information = Double
>   vzero = Bits 0
>   vnegate (Bits x) = Bits (negate x)
>   (Bits x) %+ (Bits y) = Bits $ x + y
>   k %* (Bits x) = Bits (k * x)

>instance Unit Information where
>   amount = number_of_bits
>   fromAmount = Bits
>   dimension _ = dimensionless
>   fromQuantity = fromQuantityDef dimensionless Bits
>   unitOf _ = "b"

>newtype SoundLevel = SoundAmplitude { sound_amplitude :: Double }
>  deriving (Eq, Ord, Typeable, Data, Generic)
>  deriving newtype (Binary)

>instance Show SoundLevel where { show = show_unit }
>-- | NOTE: additive operations mapped to multiplicative.
>instance VectorSpace SoundLevel where
>   type Scalar SoundLevel = Double
>   vzero = SoundAmplitude 1
>   vnegate (SoundAmplitude x) = SoundAmplitude (1/x)
>   (SoundAmplitude x) %+ (SoundAmplitude y) = SoundAmplitude (x * y)
>   k %* (SoundAmplitude x) = SoundAmplitude (x ** k)

>instance Unit SoundLevel where
>   amount x = log (sound_amplitude x) / log 10
>   fromAmount = SoundAmplitude . (10.0 **)
>   dimension _ = dimensionless
>   fromQuantity = fromQuantityDef dimensionless fromAmount
>   unitOf _ = "dB"

>newtype Angle = Radians { radians :: Double }
>   deriving (Eq,Ord, Typeable, Data, Generic)
>   deriving newtype (Binary)
>instance Show Angle where { show = show_unit }

>instance VectorSpace Angle where
>   type Scalar Angle = Double
>   vzero = Radians 0
>   vnegate (Radians x) = Radians (negate x)
>   (Radians x) %+ (Radians y) = Radians $ x + y
>   k %* (Radians x) = Radians $ k * x

>exp_angle :: Angle -> Complex Double
>exp_angle (Radians x) = (cos x :+ sin x)
>
>fromPolar :: Length -> Angle -> Complex Length
>fromPolar r (Radians alfa) = (cos alfa %* r :+ sin alfa %* r)

>toPolar :: Complex Length -> (Length, Angle)
>toPolar ((Meter a) :+ (Meter b)) = (Meter $ sqrt (a*a %+ b*b), Radians $ atan2 b a)

>newtype DegreesAngle = Degrees { degrees :: Double }
>   deriving (Eq,Ord, Typeable, Data, Generic)
>   deriving newtype (Binary)

>instance Show DegreesAngle where { show = show_unit }

>instance VectorSpace DegreesAngle where
>   type Scalar DegreesAngle = Double
>   vzero = Degrees 0
>   vnegate (Degrees x) = Degrees $ negate x
>   (Degrees x) %+ (Degrees y) = Degrees $ x + y
>   k %* (Degrees x) = Degrees $ k * x

>degreesAngle :: DegreesAngle -> Angle
>degreesAngle (Degrees d) = Radians $ (d * pi) / 180.0

>angleDegrees :: Angle -> DegreesAngle
>angleDegrees (Radians r) = Degrees $ r * 180.0 / pi

>instance Unit DegreesAngle where
>   amount = degrees
>   fromAmount = Degrees
>   dimension _ = dimensionless
>   fromQuantity = fromQuantityDef dimensionless Degrees
>   unitOf _ = "°"
>   zeroAmount _ = 0
>   conversionFactor _ = pi/180.0

>instance Unit Angle where
>   amount = radians
>   fromAmount = Radians
>   dimension _ = dimensionless
>   fromQuantity = fromQuantityDef dimensionless Radians
>   unitOf _ = "rad"

>-- | <https://en.wikipedia.org/wiki/Steradian>
>newtype SolidAngle = Steradians { steradians :: Double }
>   deriving (Eq,Ord, Typeable, Data, Generic)
>   deriving newtype (Binary)

>instance Show SolidAngle where { show = show_unit }
>instance VectorSpace SolidAngle where
>   type Scalar SolidAngle = Double
>   vzero = Steradians 0
>   vnegate (Steradians x) = Steradians (negate x)
>   (Steradians x) %+ (Steradians y) = Steradians $ x + y
>   k %* (Steradians x) = Steradians $ k * x

>instance Unit SolidAngle where
>   amount = steradians
>   fromAmount = Steradians
>   dimension _ = dimensionless
>   fromQuantity = fromQuantityDef dimensionless Steradians
>   unitOf _ = "sr"

>show_unit :: (Unit u, Show (Scalar u)) => u -> String
>show_unit i = show (amount i) ++ " " ++ unitOf i

>newtype Percentage = Percentage { percentages :: Double }
>  deriving (Eq,Ord, Typeable, Data, Generic)
>  deriving newtype (Binary)

>instance Show Percentage where { show = show_unit }
>instance VectorSpace Percentage where
>   type Scalar Percentage = Double
>   vzero = Percentage 0
>   vnegate (Percentage i) = Percentage $ negate i
>   (Percentage x) %+ (Percentage y) = Percentage $ x + y
>   k %* (Percentage x) = Percentage (k * x)
>
>instance Unit Percentage where
>  amount = percentages
>  fromAmount = Percentage
>  fromQuantity = fromQuantityDef dimensionless (Percentage . (100.0*))
>  dimension _ = dimensionless
>  unitOf _ = "%"
>  conversionFactor _ = 0.01

>newtype Acceleration = MetersPerSquareSecond { metersPerSquareSecond :: Double }
>   deriving (Eq,Ord, Typeable, Data, Generic)
>   deriving newtype (Binary)
>instance Show Acceleration where { show = show_unit }
>instance VectorSpace Acceleration where
>   type Scalar Acceleration = Double
>   vzero = MetersPerSquareSecond 0
>   vnegate (MetersPerSquareSecond x) = MetersPerSquareSecond (negate x)
>   (MetersPerSquareSecond x) %+ (MetersPerSquareSecond y) = MetersPerSquareSecond (x + y)
>   k %* (MetersPerSquareSecond x) = MetersPerSquareSecond (k %* x)

>instance Unit Acceleration where
>   amount = metersPerSquareSecond
>   fromAmount = MetersPerSquareSecond
>   fromQuantity = fromQuantityDef (meter_dimension %- (second_dimension %+ second_dimension)) MetersPerSquareSecond
>   dimension _ = meter_dimension %- (second_dimension %+ second_dimension)
>   unitOf _ = "m/s^2"

>newtype Velocity = MetersPerSecond { metersPerSecond :: Double }
>   deriving (Eq,Ord, Typeable, Data, Generic)
>   deriving newtype (Binary)
>instance Show Velocity where { show = show_unit }
>instance VectorSpace Velocity where
>   type Scalar Velocity = Double
>   vzero = MetersPerSecond 0
>   vnegate (MetersPerSecond x) = MetersPerSecond (negate x)
>   (MetersPerSecond x) %+ (MetersPerSecond y) = MetersPerSecond $ x + y
>   k %* (MetersPerSecond x) = MetersPerSecond (k * x)
>
>instance Unit Velocity where
>   amount = metersPerSecond
>   fromAmount = MetersPerSecond
>   fromQuantity = fromQuantityDef (meter_dimension %- second_dimension) MetersPerSecond
>   dimension _ = meter_dimension %- second_dimension
>   unitOf _ = "m/s"

>newtype SquareLength = SquareMeter { squaremeters :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show SquareLength where { show = show_unit }
>instance VectorSpace SquareLength where
>   type Scalar SquareLength = Double
>   vzero = SquareMeter 0
>   vnegate (SquareMeter x) = SquareMeter $ negate x
>   (SquareMeter x) %+ (SquareMeter y) = SquareMeter $ negate x
>   k %* (SquareMeter x) = SquareMeter $ k * x
>instance Unit SquareLength where
>   amount = squaremeters
>   fromAmount = SquareMeter
>   fromQuantity = fromQuantityDef squaremeter_dimension SquareMeter
>   dimension _ = meter_dimension %+ meter_dimension
>   unitOf _ = "m^2"

>-- types for basic units <https://en.wikipedia.org/wiki/International_System_of_Units>
>newtype Length = Meter { meters :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Length where { show = show_unit }
>instance VectorSpace Length where
>   type Scalar Length = Double
>   vzero = Meter 0
>   vnegate (Meter x) = Meter $ negate x
>   (Meter x) %+ (Meter y) = Meter $ x + y
>   k %* (Meter x) = Meter $ k * x

>newtype Mass = Kilogram { kilograms :: Double }
>  deriving (Eq,Ord, Typeable, Data, Generic)
>  deriving newtype (Binary)
>instance Show Mass where { show = show_unit }
>instance VectorSpace Mass where
>   type Scalar Mass = Double
>   vzero = Kilogram 0
>   vnegate (Kilogram x) = Kilogram $ negate x
>   (Kilogram x) %+ (Kilogram y) = Kilogram $ x + y
>   k %* (Kilogram x) = Kilogram $ k * x
>newtype Time = Second { seconds :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Time where { show = show_unit }

>instance VectorSpace Time where
>   type Scalar Time = Double
>   vzero = Second 0
>   vnegate (Second x) = Second $ negate x
>   (Second x) %+ (Second y) = Second $ x + y
>   k %* (Second x) = Second $ k * x
>newtype Current = Ampere { amperes :: Double }
>  deriving (Eq,Ord, Typeable, Data, Generic)
>  deriving newtype (Binary)
>instance Show Current where { show = show_unit }

>instance VectorSpace Current where
>   type Scalar Current = Double
>   vzero = Ampere 0
>   vnegate (Ampere x) = Ampere $ negate x
>   (Ampere x) %+ (Ampere y) = Ampere $ x + y
>   k %* (Ampere x) = Ampere $ k * x
>

>-- | <https://en.wikipedia.org/wiki/Fahrenheit>
>newtype DegreesFahrenheit = Fahrenheit { fahrenheits :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show DegreesFahrenheit where { show = show_unit }
>instance VectorSpace DegreesFahrenheit where
>   type Scalar DegreesFahrenheit = Double
>   vzero = Fahrenheit 0
>   vnegate (Fahrenheit x) = Fahrenheit $ negate x
>   (Fahrenheit x) %+ (Fahrenheit y) = Fahrenheit $ x + y
>   k %* (Fahrenheit x) = Fahrenheit $ k * x
>instance Unit DegreesFahrenheit where
>   amount = fahrenheits
>   fromAmount = Fahrenheit
>   dimension _ = kelvin_dimension
>   unitOf _ = "°F"
>   fromQuantity x
>     | dimension x == kelvin_dimension = return $ Fahrenheit $ (amount x * (9/5) - 459.67)
>     | otherwise = invalidDimensions "fromQuantity:DegreesFahrenheit" (dimension x) kelvin_dimension (amount x) (amount x)
>   zeroAmount _ = 459.67
>   conversionFactor _ = 5/9

>newtype DegreesTemperature = Celcius { celciuses :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show DegreesTemperature where { show = show_unit }
>instance VectorSpace DegreesTemperature where
>   type Scalar DegreesTemperature = Double
>   vzero = Celcius 0
>   vnegate (Celcius x) = Celcius $ negate x
>   (Celcius x) %+ (Celcius y) = Celcius $ x + y
>   k %* (Celcius x) = Celcius $ k * x
>
>instance Unit DegreesTemperature where
>   amount = celciuses
>   fromAmount = Celcius
>   dimension _ = kelvin_dimension
>   unitOf _ = "°C"
>   fromQuantity x
>     | dimension x == kelvin_dimension = return $ Celcius $ amount (x - (273.15 @@ kelvin_dimension))
>     | otherwise = invalidDimensions "fromQuantity:DegreesTemperature" (dimension x) kelvin_dimension (amount x) (amount x)
>   zeroAmount _ = 273.15

>newtype Temperature = Kelvin { kelvins :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Temperature where { show = show_unit }

>instance VectorSpace Temperature where
>   type Scalar Temperature = Double
>   vzero = Kelvin 0
>   vnegate (Kelvin x) = Kelvin $ negate x
>   (Kelvin x) %+ (Kelvin y) = Kelvin $ x + y
>   k %* (Kelvin x) = Kelvin $ k * x
>newtype Substance = Mole { moles :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Substance where { show = show_unit }

>instance VectorSpace Substance where
>   type Scalar Substance = Double
>   vzero = Mole 0
>   vnegate (Mole x) = Mole $ negate x
>   (Mole x) %+ (Mole y) = Mole $ x + y
>   k %* (Mole x) = Mole $ k * x
>newtype Intensity = Candela { candelas :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Intensity where { show = show_unit }

>instance VectorSpace Intensity where
>   type Scalar Intensity = Double
>   vzero = Candela 0
>   vnegate (Candela x) = Candela $ negate x
>   (Candela x) %+ (Candela y) = Candela $ x + y
>   k %* (Candela x) = Candela $ k * x
>newtype Frequency = Hertz { hertzs :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Frequency where { show = show_unit }

>instance VectorSpace Frequency where
>   type Scalar Frequency = Double
>   vzero = Hertz 0
>   vnegate (Hertz x) = Hertz $ negate x
>   (Hertz x) %+ (Hertz y) = Hertz $ x + y
>   k %* (Hertz x) = Hertz $ k * x
>newtype Force = Newton { newtons :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Force where { show = show_unit }

>instance VectorSpace Force where
>   type Scalar Force = Double
>   vzero = Newton 0
>   vnegate (Newton x) = Newton $ negate x
>   (Newton x) %+ (Newton y) = Newton $ x + y
>   k %* (Newton x) = Newton $ k * x
>newtype Pressure = Pascal { pascals :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Pressure where { show = show_unit }

>instance VectorSpace Pressure where
>   type Scalar Pressure = Double
>   vzero = Pascal 0
>   vnegate (Pascal x) = Pascal $ negate x
>   (Pascal x) %+ (Pascal y) = Pascal $ x + y
>   k %* (Pascal x) = Pascal $ k * x
>newtype Energy = Joule { joules :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Energy where { show = show_unit }

>instance VectorSpace Energy where
>   type Scalar Energy = Double
>   vzero = Joule 0
>   vnegate (Joule x) = Joule $ negate x
>   (Joule x) %+ (Joule y) = Joule $ x + y
>   k %* (Joule x) = Joule $ k * x
>newtype Power = Watt { watts :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Power where { show = show_unit }

>instance VectorSpace Power where
>   type Scalar Power = Double
>   vzero = Watt 0
>   vnegate (Watt x) = Watt $ negate x
>   (Watt x) %+ (Watt y) = Watt $ x + y
>   k %* (Watt x) = Watt $ k * x
>newtype Charge = Coulomb { coulombs :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Charge where { show = show_unit }

>instance VectorSpace Charge where
>   type Scalar Charge = Double
>   vzero = Coulomb 0
>   vnegate (Coulomb x) = Coulomb $ negate x
>   (Coulomb x) %+ (Coulomb y) = Coulomb $ x + y
>   k %* (Coulomb x) = Coulomb $ k * x
>newtype Voltage = Volt { volts :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Voltage where { show = show_unit }

>instance VectorSpace Voltage where
>   type Scalar Voltage = Double
>   vzero = Volt 0
>   vnegate (Volt x) = Volt $ negate x
>   (Volt x) %+ (Volt y) = Volt $ x + y
>   k %* (Volt x) = Volt $ k * x
>newtype Capacitance = Farad { farads :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic) deriving newtype (Binary)
>instance Show Capacitance where { show = show_unit }

>instance VectorSpace Capacitance where
>   type Scalar Capacitance = Double
>   vzero = Farad 0
>   vnegate (Farad x) = Farad $ negate x
>   (Farad x) %+ (Farad y) = Farad $ x + y
>   k %* (Farad x) = Farad $ k * x
>newtype Resistance = Ohm { ohms :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Resistance where { show = show_unit }

>instance VectorSpace Resistance where
>   type Scalar Resistance = Double
>   vzero = Ohm 0
>   vnegate (Ohm x) = Ohm $ negate x
>   (Ohm x) %+ (Ohm y) = Ohm $ x + y
>   k %* (Ohm x) = Ohm $ k * x
>newtype Conductance = Siemens { siemenses :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic) deriving newtype (Binary)
>instance Show Conductance where { show = show_unit }

>instance VectorSpace Conductance where
>   type Scalar Conductance = Double
>   vzero = Siemens 0
>   vnegate (Siemens x) = Siemens $ negate x
>   (Siemens x) %+ (Siemens y) = Siemens $ x + y
>   k %* (Siemens x) = Siemens $ k * x
>newtype Flux = Weber { webers :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic) deriving newtype (Binary)
>instance Show Flux where { show = show_unit }

>instance VectorSpace Flux where
>   type Scalar Flux = Double
>   vzero = Weber 0
>   vnegate (Weber x) = Weber $ negate x
>   (Weber x) %+ (Weber y) = Weber $ x + y
>   k %* (Weber x) = Weber $ k * x
>newtype FluxDensity = Tesla { teslas :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic) deriving newtype (Binary)
>instance Show FluxDensity where { show = show_unit }

>instance VectorSpace FluxDensity where
>   type Scalar FluxDensity = Double
>   vzero = Tesla 0
>   vnegate (Tesla x) = Tesla $ negate x
>   (Tesla x) %+ (Tesla y) = Tesla $ x + y
>   k %* (Tesla x) = Tesla $ k * x
>newtype Inductance  = Henry { henrys :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Inductance where { show = show_unit }

>instance VectorSpace Inductance where
>   type Scalar Inductance = Double
>   vzero = Henry 0
>   vnegate (Henry x) = Henry $ negate x
>   (Henry x) %+ (Henry y) = Henry $ x + y
>   k %* (Henry x) = Henry $ k * x
>newtype LuminousFlux = Lumen { lumens :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic) deriving newtype (Binary)
>instance Show LuminousFlux where { show = show_unit }

>instance VectorSpace LuminousFlux where
>   type Scalar LuminousFlux = Double
>   vzero = Lumen 0
>   vnegate (Lumen x) = Lumen $ negate x
>   (Lumen x) %+ (Lumen y) = Lumen $ x + y
>   k %* (Lumen x) = Lumen $ k * x
>newtype Illuminance = Lux { luxes :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Illuminance where { show = show_unit }

>instance VectorSpace Illuminance where
>   type Scalar Illuminance = Double
>   vzero = Lux 0
>   vnegate (Lux x) = Lux $ negate x
>   (Lux x) %+ (Lux y) = Lux $ x + y
>   k %* (Lux x) = Lux $ k * x
>newtype Radioactivity = Becquerel { becquerels :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show Radioactivity where { show = show_unit }

>instance VectorSpace Radioactivity where
>   type Scalar Radioactivity = Double
>   vzero = Becquerel 0
>   vnegate (Becquerel x) = Becquerel $ negate x
>   (Becquerel x) %+ (Becquerel y) = Becquerel $ x + y
>   k %* (Becquerel x) = Becquerel $ k * x
>newtype AbsorbedDose  = Gray { grays :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show AbsorbedDose where { show = show_unit }

>instance VectorSpace AbsorbedDose where
>   type Scalar AbsorbedDose = Double
>   vzero = Gray 0
>   vnegate (Gray x) = Gray $ negate x
>   (Gray x) %+ (Gray y) = Gray $ x + y
>   k %* (Gray x) = Gray $ k * x
>newtype EquivalentDose = Sievert { sieverts :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)

>instance Show EquivalentDose where { show = show_unit }

>instance VectorSpace EquivalentDose where
>   type Scalar EquivalentDose = Double
>   vzero = Sievert 0
>   vnegate (Sievert x) = Sievert $ negate x
>   (Sievert x) %+ (Sievert y) = Sievert $ x + y
>   k %* (Sievert x) = Sievert $ k * x

>newtype CatalyticActivity = Katal { katals :: Double }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance Show CatalyticActivity where { show = show_unit }

>instance VectorSpace CatalyticActivity where
>   type Scalar CatalyticActivity = Double
>   vzero = Katal 0
>   vnegate (Katal x) = Katal $ negate x
>   (Katal x) %+ (Katal y) = Katal $ x + y
>   k %* (Katal x) = Katal $ k * x
>instance Unit CatalyticActivity where
>   amount = katals
>   fromAmount = Katal
>   dimension _ = katal_dimension
>   fromQuantity = fromQuantityDef katal_dimension Katal
>   unitOf _ = "kat"
>instance Unit EquivalentDose where
>   amount = sieverts
>   fromAmount = Sievert
>   dimension _ = sievert_dimension
>   fromQuantity = fromQuantityDef sievert_dimension Sievert
>   unitOf _ = "Sv"
>   
>                                     
>instance Unit AbsorbedDose where
>   amount = grays
>   fromAmount = Gray
>   dimension _ = gray_dimension
>   fromQuantity = fromQuantityDef gray_dimension Gray
>   unitOf _ = "Gy"
>   
>instance Unit Radioactivity where
>   amount = becquerels
>   fromAmount = Becquerel
>   dimension _ = becquerel_dimension
>   fromQuantity = fromQuantityDef becquerel_dimension Becquerel
>   unitOf _ = "Bq"
>instance Unit Illuminance where
>   amount = luxes
>   fromAmount = Lux
>   dimension _ = lux_dimension
>   fromQuantity = fromQuantityDef lux_dimension Lux
>   unitOf _ = "lx"
>instance Unit LuminousFlux where
>   amount = lumens
>   fromAmount = Lumen
>   dimension _ = lumen_dimension
>   fromQuantity = fromQuantityDef lumen_dimension Lumen
>   unitOf _ = "lm"
>instance Unit Inductance where
>   amount = henrys
>   fromAmount = Henry
>   dimension _ = henry_dimension
>   fromQuantity = fromQuantityDef henry_dimension Henry
>   unitOf _ = "H"
>instance Unit FluxDensity where
>   amount = teslas
>   fromAmount = Tesla
>   dimension _ = tesla_dimension
>   fromQuantity = fromQuantityDef tesla_dimension Tesla
>   unitOf _ = "T"
>instance Unit Flux where
>   amount = webers
>   fromAmount = Weber
>   dimension _ = weber_dimension
>   fromQuantity = fromQuantityDef weber_dimension Weber
>   unitOf _ = "W"
>instance Unit Conductance where
>   amount = siemenses
>   fromAmount = Siemens
>   dimension _ = siemens_dimension
>   fromQuantity = fromQuantityDef siemens_dimension Siemens
>   unitOf _ = "S"
>instance Unit Resistance where
>   amount = ohms
>   fromAmount = Ohm
>   dimension _ = ohm_dimension
>   fromQuantity = fromQuantityDef ohm_dimension Ohm
>   unitOf _ = "Ω"
>instance Unit Capacitance where
>   amount = farads
>   fromAmount = Farad
>   dimension _ = farad_dimension
>   fromQuantity = fromQuantityDef farad_dimension Farad
>   unitOf _ = "F"
>instance Unit Voltage where
>   amount = volts
>   fromAmount = Volt
>   dimension _ = volt_dimension
>   fromQuantity = fromQuantityDef volt_dimension Volt
>   unitOf _ = "V"
>instance Unit Charge where
>   amount = coulombs
>   fromAmount = Coulomb
>   dimension _ = coulomb_dimension
>   fromQuantity = fromQuantityDef coulomb_dimension Coulomb
>   unitOf _ = "C"
>instance Unit Power where
>   amount = watts
>   fromAmount = Watt
>   dimension _ = watt_dimension
>   fromQuantity = fromQuantityDef watt_dimension Watt
>   unitOf _ = "W"
>instance Unit Energy where
>   amount = joules
>   fromAmount = Joule
>   dimension _ = joule_dimension
>   fromQuantity = fromQuantityDef joule_dimension Joule
>   unitOf _ = "J"
>instance Unit Pressure where
>   amount = pascals
>   fromAmount = Pascal
>   dimension _ = pascal_dimension
>   fromQuantity = fromQuantityDef pascal_dimension Pascal
>   unitOf _ = "Pa"
>instance Unit Force where
>   amount = newtons
>   fromAmount = Newton
>   dimension _ = newton_dimension
>   fromQuantity = fromQuantityDef newton_dimension Newton
>   unitOf _ = "N"
>instance Unit Frequency where
>   amount = hertzs
>   fromAmount = Hertz
>   dimension _ = hertz_dimension
>   fromQuantity = fromQuantityDef hertz_dimension Hertz
>   unitOf _ = "Hz"
>instance Unit Length where
>   amount = meters
>   fromAmount = Meter
>   dimension _ = meter_dimension
>   fromQuantity = fromQuantityDef meter_dimension Meter
>   unitOf _ = "m"
>instance Unit Mass where
>   amount = kilograms
>   fromAmount = Kilogram
>   dimension _ = kilogram_dimension
>   fromQuantity = fromQuantityDef kilogram_dimension Kilogram
>   unitOf _ = "kg"
>instance Unit Time where
>   amount = seconds
>   fromAmount = Second
>   dimension _ = second_dimension
>   fromQuantity = fromQuantityDef second_dimension Second
>   unitOf _ = "s"
>instance Unit Current where
>   amount = amperes
>   fromAmount = Ampere
>   dimension _ = ampere_dimension
>   fromQuantity = fromQuantityDef ampere_dimension Ampere
>   unitOf _ = "A"
>instance Unit Temperature where
>   amount = kelvins
>   fromAmount = Kelvin
>   dimension _ = kelvin_dimension
>   fromQuantity = fromQuantityDef kelvin_dimension Kelvin
>   unitOf _ = "K"
>
> 
>instance Unit Substance where
>   amount = moles
>   fromAmount = Mole
>   dimension _ = mol_dimension
>   fromQuantity = fromQuantityDef mol_dimension Mole
>   unitOf _ = "mol"
>instance Unit Intensity where
>   amount = candelas
>   fromAmount = Candela
>   dimension _ = candela_dimension
>   fromQuantity = fromQuantityDef candela_dimension Candela
>   unitOf _ = "cd"

