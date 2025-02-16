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

>data a :/ b  = QDivide { qdivideAmount :: !(Scalar a),
>                         qdivideDividendUnit :: UnitName a,
>                         qdivideDivisorUnit  :: UnitName b }
>   deriving (Typeable, Generic)

>data a :* b = QProduct { qproductAmount :: !(Scalar a),
>                         qproduct_firstUnit :: UnitName a,
>                         qproduct_secondUnit :: UnitName b
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
>   amount = qproductAmount
>   unitOf z@(QProduct x _ _) = show (dimension z)
>   fromQuantity q = do
>     let res = QProduct (valueAmount q) fromAmount fromAmount
>     guard (valueDimension q == dimension res)
>       <|> invalidDimensions "fromQuantity" (valueDimension q) (dimension res)
>                                           (valueAmount q) (valueAmount q)
>     return res
>   dimension (QProduct x s t) = unitNameToDimension s + unitNameToDimension t

>instance (Scalar a ~ Scalar b, Show (Scalar a), LiteralUnit a, LiteralUnit b) => LiteralUnit (a :* b) where
>   fromAmount d = QProduct d fromAmount fromAmount 

>instance (LiteralUnit a, LiteralUnit b, Show (Scalar a), Scalar a ~ Scalar b) => Unit (a :/ b) where
>   amount = qdivideAmount
>   unitOf z@(QDivide x _ _) = show (dimension z)
>   fromQuantity q = do
>      let res = QDivide (valueAmount q) fromAmount fromAmount 
>      guard (valueDimension q == dimension res)
>        <|> invalidDimensions "fromQuantity" (valueDimension q) (dimension res)
>                                            (valueAmount q) (valueAmount q)
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

>newtype Dimensionless a = Dimensionless { dimensionlessValue :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary, Num, Fractional, Real, RealFrac, Floating, RealFloat)
>instance (Num a, Show a) => Show (Dimensionless a) where { show = showUnit }
>instance (Show a, Read a) => Read (Dimensionless a) where
>   readPrec = readPrec >>= (return . Dimensionless)
>instance (Num a) => VectorSpace (Dimensionless a) where
>   type Scalar (Dimensionless a) = a
>   vzero = Dimensionless 0
>   vnegate (Dimensionless x) = Dimensionless (negate x)
>   (Dimensionless x) %+ (Dimensionless y) = Dimensionless $ x + y
>   k %* (Dimensionless x) = Dimensionless $ k * x


>instance (Num a) => NormedSpace (Dimensionless a) where
>   norm = amount
>   normSquared (Dimensionless x) = x*x

>instance (Num a) => Unit (Dimensionless a) where
>   amount = dimensionlessValue
>   unitOf _ = ""
>   dimension _ = dimensionless
>   fromQuantity (x `As` d) = do
>     guard (isDimensionless d)
>     return $ Dimensionless x

>instance (Show a, Num a) => LiteralUnit (Dimensionless a) where
>   fromAmount = Dimensionless

>newtype Information a = Bits { numberOfBits :: a }
>  deriving (Eq,Ord, Data,Generic, Typeable)
>  deriving newtype (Binary)
>instance (Num a, Show a) => Show ( Information  a) where { show = showUnit }

>instance (Num a, Show a) => Unit (Information a) where
>   amount = numberOfBits
>   dimension _ = dimensionless
>   fromQuantity = fromQuantityDef dimensionless Bits
>   unitOf _ = "b"
>instance (Show a, Num a) => LiteralUnit (Information a) where { fromAmount = Bits }
> 
>newtype SoundLevel a = SoundAmplitude { soundAmplitude :: a }
>  deriving (Eq, Ord, Typeable, Data, Generic)
>  deriving newtype (Binary)

>instance (Floating a, Show a) => Show ( SoundLevel  a) where { show = showUnit }
>-- | NOTE: additive operations mapped to multiplicative.
>-- Notice this reduces the possible range of SoundLevel values
>-- to around -300..300 dB based on possible exponents in Double.
>instance (Floating a) => VectorSpace (SoundLevel a) where
>   type Scalar (SoundLevel a) = a
>   vzero = SoundAmplitude 1
>   vnegate (SoundAmplitude x) = SoundAmplitude (1/x)
>   (SoundAmplitude x) %+ (SoundAmplitude y) = SoundAmplitude (x * y)
>   k %* (SoundAmplitude x) = SoundAmplitude (x ** k)

>instance (Floating a, Show a) => Unit (SoundLevel a) where
>   amount x = log (soundAmplitude x) / log 10
>   dimension _ = dimensionless
>   fromQuantity = fromQuantityDef dimensionless fromAmount
>   unitOf _ = "dB"
>instance (Show a, Floating a) => LiteralUnit (SoundLevel a) where { fromAmount = SoundAmplitude . (10.0 **) }

>newtype Angle a = Radians { radians :: a }
>   deriving (Eq,Ord, Typeable, Data, Generic)
>   deriving newtype (Binary, Num, Fractional)

>instance Functor Angle where
>  fmap f (Radians x) = Radians (f x)

>instance (Show a, Num a) => Show (Angle a) where { show = showUnit }

>class (Functor angle) => DimensionalFloating angle where
>  halfRotation :: angle Double
>  fullRotation :: angle Double
>  fractionOfRotation :: Rational -> angle Double
>  dimSin :: (Floating a) => angle a -> a
>  dimCos :: (Floating a) => angle a -> a
>  dimTan :: (Floating a) => angle a -> a
>  dimAsin :: (Floating a) => a -> angle a
>  dimAcos :: (Floating a) => a -> angle a
>  dimAtan :: (Floating a) => a -> angle a
>  dimLog :: (RealFloat a) => Complex a -> angle a
>  dimExp :: (RealFloat a) => angle a -> Complex a
>  dimFromPolar :: (RealFloat a) => a -> angle a -> Complex a
>  dimToPolar :: (RealFloat a) => Complex a -> (a, angle a)
>  fullRotation = fractionOfRotation (1 % 1)
>  halfRotation = fractionOfRotation (1 % 2)

>instance DimensionalFloating Angle where
>  halfRotation = piAngle
>  fullRotation = fromRational 2 %* halfRotation
>  fractionOfRotation q = fromRational q %* fullRotation
>  dimSin = sinAngle
>  dimCos = cosAngle
>  dimTan = tanAngle
>  dimAsin = asinAngle
>  dimAcos = acosAngle
>  dimAtan = atanAngle
>  dimLog = logAngle
>  dimExp = expAngle
>  dimFromPolar r (Radians alfa) = (r * cos alfa) :+ (r * sin alfa)
>  dimToPolar (a :+ b) = (sqrt (a*a+b*b), Radians (atan2 b a))
> 
>piAngle :: Angle Double
>piAngle = Radians pi

>fraction_of_circle :: Rational -> Angle Double
>fraction_of_circle r = fromRational (2*r) %* piAngle

>asinAngle :: (Floating a) => a -> Angle a
>asinAngle = Radians . asin

>acosAngle :: (Floating a) => a -> Angle a
>acosAngle = Radians . acos
>
>atanAngle :: (Floating a) => a -> Angle a
>atanAngle = Radians . atan

>sinAngle :: (Floating a) => Angle a -> a
>sinAngle = sin . radians

>cosAngle :: (Floating a) => Angle a -> a
>cosAngle = cos . radians
>
>tanAngle :: (Floating a) => Angle a -> a
>tanAngle = tan . radians

>logAngle :: (RealFloat a) => Complex a -> Angle a
>logAngle = Radians . phase

>expAngle :: (Floating a) => Angle a -> Complex a
>expAngle (Radians x) = (cos x :+ sin x)
>
>fromPolar :: (Unit u, Floating (Scalar u)) => u -> Angle (Scalar u) -> Complex u
>fromPolar r (Radians alfa) = (cos alfa %* r :+ sin alfa %* r)

>toPolar :: (LiteralUnit u, RealFloat (Scalar u)) => Complex u -> (u, Angle (Scalar u))
>toPolar z@((a :: u) :+ b) = (fromAmount (sqrt (a'*a' + b'*b')) :: u , Radians $ atan2 b' a')
>   where a' = amount a
>         b' = amount b

>newtype DegreesAngle a = Degrees { degrees :: a }
>   deriving (Eq,Ord, Typeable, Data, Generic)
>   deriving newtype (Binary, Num, Fractional)


>instance (Show a, Floating a) => Show (DegreesAngle a) where { show = showUnit }


>degreesAngle :: (Floating a) => DegreesAngle a -> Angle a
>degreesAngle (Degrees d) = Radians $ (d * pi) / 180.0

>angleDegrees :: (Floating a) => Angle a -> DegreesAngle a
>angleDegrees (Radians r) = Degrees $ r * 180.0 / pi

>isoDegreesToAngle :: (Floating a) => DegreesAngle a :==: Angle a
>isoDegreesToAngle = degreesAngle <-> angleDegrees

>instance (Show a, Floating a) => Unit (DegreesAngle a) where
>   amount = degrees
>   dimension _ = dimensionless
>   fromQuantity = fromQuantityDef dimensionless Degrees
>   unitOf _ = "°"
>instance (Show a, Floating a) => LiteralUnit (DegreesAngle a) where
>   fromAmount = Degrees
>   zeroAmount _ = 0
>   conversionFactor _ = pi/180.0


>instance (Num a, Show a) => Unit (Angle a) where
>   amount = radians
>   dimension _ = dimensionless
>   fromQuantity = fromQuantityDef dimensionless Radians
>   unitOf _ = "rad"

>instance (Num a, Show a) => LiteralUnit (Angle a) where { fromAmount = Radians }

>-- | <https://en.wikipedia.org/wiki/Steradian>
>newtype SolidAngle a = Steradians { steradians :: a }
>   deriving (Eq,Ord, Typeable, Data, Generic)
>   deriving newtype (Binary, Num, Fractional)

>instance (Num a, Show a) => Show ( SolidAngle  a) where { show = showUnit }

>instance (Num a, Show a) => Unit (SolidAngle a) where
>   amount = steradians
>   dimension _ = dimensionless
>   fromQuantity = fromQuantityDef dimensionless Steradians
>   unitOf _ = "sr"
>instance (Show a, Num a) => LiteralUnit (SolidAngle a) where { fromAmount = Steradians }

>showUnit :: (Unit u, Show (Scalar u)) => u -> String
>showUnit i = show (amount i) ++ " " ++ unitOf i

>readUnit :: (Unit u, Read (Scalar u)) => UnitName u -> Text.ParserCombinators.ReadPrec.ReadPrec u
>readUnit unitname = readPrec >>= \d -> lift skipSpaces
> >> lift (string $ unitOf $ unitname d)
> >> return (unitname d)

>newtype Percentage a = Percentages { percentages :: a }
>  deriving (Eq,Ord, Typeable, Data, Generic)
>  deriving newtype (Binary, Num, Fractional)

>instance (Fractional a, Show a) => Show (Percentage a) where { show = showUnit }
>
>instance (Fractional a, Show a) => Unit (Percentage a) where
>  amount = percentages
>  fromQuantity = fromQuantityDef dimensionless (Percentages . (100.0*))
>  dimension _ = dimensionless
>  unitOf _ = "%"
>instance (Show a, Fractional a) => LiteralUnit (Percentage a) where
>   fromAmount = Percentages
>   conversionFactor _ = 0.01
>
>newtype Acceleration a = MetersPerSquareSecond { metersPerSquareSecond :: a }
>   deriving (Eq,Ord, Typeable, Data, Generic)
>   deriving newtype (Binary)

>instance (Num a, Show a) => Show (Acceleration a) where { show = showUnit }

>instance (Num a, Show a) => Unit (Acceleration a) where
>   amount = metersPerSquareSecond
>   fromQuantity = fromQuantityDef (meterDimension %- (secondDimension %+ secondDimension)) MetersPerSquareSecond
>   dimension _ = meterDimension %- (secondDimension %+ secondDimension)
>   unitOf _ = "m/s^2"
>instance (Show a, Num a) => LiteralUnit (Acceleration a) where { fromAmount = MetersPerSquareSecond }
>newtype Velocity a = MetersPerSecond { metersPerSecond :: a }
>   deriving (Eq,Ord, Typeable, Data, Generic)
>   deriving newtype (Binary)
>instance (Num a, Show a) => Show (Velocity a) where { show = showUnit }
>
>instance (Num a, Show a) => Unit (Velocity a) where
>   amount = metersPerSecond
>   fromQuantity = fromQuantityDef (meterDimension %- secondDimension) MetersPerSecond
>   dimension _ = meterDimension %- secondDimension
>   unitOf _ = "m/s"
>instance (Show a, Num a) => LiteralUnit (Velocity a) where { fromAmount = MetersPerSecond }
>newtype SquareLength a = SquareMeters { squaremeters :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)

>newtype CubicLength a = CubicMeters { cubicmeters :: a }
>  deriving (Eq,Ord, Typeable, Data, Generic)
>  deriving newtype (Binary)
>
>sqrt_length :: (Floating a) => SquareLength a -> Length a
>sqrt_length (SquareMeters m) = Meters (sqrt m)

>square_length :: (Num a) => Length a -> SquareLength a
>square_length (Meters m) = SquareMeters (m * m)

>times_length :: (Num a) => Length a -> Length a -> SquareLength a
>times_length (Meters m1) (Meters m2) = SquareMeters (m1 * m2)

>instance (Num a, Show a) => Show (CubicLength a) where { show = showUnit }
>instance (Num a, Show a) => Unit (CubicLength a) where
>   amount = cubicmeters
>   fromQuantity = fromQuantityDef cubicmeterDimension CubicMeters
>   dimension _ = 3 %* meterDimension
>   unitOf _ = "m^3"
>instance (Show a, Num a) => LiteralUnit (CubicLength a) where { fromAmount = CubicMeters }
>instance (Num a, Show a) => Show (SquareLength a) where { show = showUnit }
>instance (Num a, Show a) => Unit (SquareLength a) where
>   amount = squaremeters
>   fromQuantity = fromQuantityDef squaremeterDimension SquareMeters
>   dimension _ = meterDimension %+ meterDimension
>   unitOf _ = "m^2"
>instance (Show a, Num a) => LiteralUnit (SquareLength a) where { fromAmount = SquareMeters }
>-- types for basic units <https://en.wikipedia.org/wiki/International_System_of_Units>
>newtype Length a = Meters { meters :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show (Length a) where { show = showUnit }

>newtype Mass a = Kilograms { kilograms :: a }
>  deriving (Eq,Ord, Typeable, Data, Generic)
>  deriving newtype (Binary)
>instance (Num a, Show a) => Show ( Mass  a) where { show = showUnit }
>newtype Time a = Seconds { seconds :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show (Time a) where { show = showUnit }

>newtype Current a = Amperes { amperes :: a }
>  deriving (Eq,Ord, Typeable, Data, Generic)
>  deriving newtype (Binary)
>instance (Num a, Show a) => Show (Current a) where { show = showUnit }

>-- | <https://en.wikipedia.org/wiki/Fahrenheit>
>newtype DegreesFahrenheit a = DegreesFahrenheit { fahrenheits :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Fractional a, Show a) => Show (DegreesFahrenheit a) where { show = showUnit }
>instance (Fractional a, Show a) => Unit (DegreesFahrenheit a) where
>   amount = fahrenheits
>   dimension _ = kelvinDimension
>   unitOf _ = "°F"
>   fromQuantity x
>     | valueDimension x == kelvinDimension = return $ DegreesFahrenheit $
>                            (valueAmount x * (9/5) - 459.67)
>     | otherwise = invalidDimensions "fromQuantity:DegreesFahrenheit" (valueDimension x) kelvinDimension (valueAmount x) (valueAmount x)
>instance (Show a, Fractional a) => LiteralUnit (DegreesFahrenheit a) where
>  fromAmount = DegreesFahrenheit
>  conversionFactor _ = 5/9
>  zeroAmount _ = 459.67
>
>-- | <https://en.wikipedia.org/wiki/Rankine_scale>
>newtype DegreesRankine a = DegreesRankine { rankines :: a }
> deriving (Eq, Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Fractional a, Show a) => Show (DegreesRankine a) where { show = showUnit }
>instance (Fractional a, Show a) => Unit (DegreesRankine a) where
>  amount = rankines
>  dimension _ = kelvinDimension
>  unitOf _ = "°R"
>  fromQuantity x
>    | valueDimension x == kelvinDimension = return $ DegreesRankine $ toRankine x
>    | otherwise = invalidDimensions "fromQuantity:DegreesRankine" (valueDimension x) kelvinDimension (valueAmount x) (valueAmount x)

>instance (Fractional a, Show a) => LiteralUnit (DegreesRankine a) where
>  fromAmount = DegreesRankine
>  conversionFactor _ = 5/9
>  zeroAmount _ = 0.0 

>newtype DegreesTemperature a = DegreesCelcius { celciuses :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Fractional a, Show a) => Show (DegreesTemperature a) where { show = showUnit }
>
>instance (Fractional a, Show a) => Unit (DegreesTemperature a) where
>   amount = celciuses
>   dimension _ = kelvinDimension
>   unitOf _ = "°C"
>   fromQuantity x
>     | valueDimension x == kelvinDimension = return $ DegreesCelcius $ valueAmount (x - (273.15 @@ kelvinDimension))
>     | otherwise = invalidDimensions "fromQuantity:DegreesTemperature" (valueDimension x) kelvinDimension (valueAmount x) (valueAmount x)

>instance (Show a, Fractional a) => LiteralUnit (DegreesTemperature a) where
>   fromAmount = DegreesCelcius
>   zeroAmount _ = 273.15

>newtype Temperature a = DegreesKelvin { kelvins :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show (Temperature a) where { show = showUnit }

>isoCelciusToKelvin :: (Fractional a) => DegreesTemperature a :==: Temperature a
>isoCelciusToKelvin = celciusToKelvin <-> kelvinToCelcius
>
>celciusToKelvin :: (Fractional a) => DegreesTemperature a -> Temperature a
>celciusToKelvin (DegreesCelcius x) = DegreesKelvin (x + 273.15)

>kelvinToCelcius :: (Fractional a) => Temperature a -> DegreesTemperature a
>kelvinToCelcius (DegreesKelvin x) = DegreesCelcius (x - 273.15)

>newtype Substance a = Moles { moles :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show (Substance a) where { show = showUnit }

>newtype Intensity a = Candelas { candelas :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show (Intensity a) where { show = showUnit }

>newtype Frequency a = Hertzes { hertzes :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show (Frequency a) where { show = showUnit }

>newtype Force a = Newtons { newtons :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show (Force a) where { show = showUnit }

>newtype Torque a = NewtonMeters { newtonmeters :: a }
>  deriving (Eq, Ord, Typeable, Data, Generic)
>  deriving newtype (Binary)
>instance (Num a, Show a) => Show (Torque a) where { show = showUnit }

>newtype Pressure a = Pascals { pascals :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show ( Pressure  a) where { show = showUnit }

>newtype Energy a = Joules { joules :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show (Energy a) where { show = showUnit }

>newtype Power a = Watts { watts :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show (Power a) where { show = showUnit }

>newtype Charge a = Coulombs { coulombs :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show (Charge a) where { show = showUnit }

>newtype Voltage a = Volts { volts :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show (Voltage a) where { show = showUnit }

>newtype Capacitance a = Farads { farads :: a }
> deriving (Eq,Ord, Typeable, Data, Generic) deriving newtype (Binary)
>instance (Num a, Show a) => Show (Capacitance a) where { show = showUnit }

>newtype Resistance a = Ohms { ohms :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show (Resistance a) where { show = showUnit }

>newtype Conductance a = Siemenses { siemenses :: a }
> deriving (Eq,Ord, Typeable, Data, Generic) deriving newtype (Binary)
>instance (Num a, Show a) => Show ( Conductance  a) where { show = showUnit }

>newtype Flux a = Webers { webers :: a }
> deriving (Eq,Ord, Typeable, Data, Generic) deriving newtype (Binary)
>instance (Num a, Show a) => Show (Flux a) where { show = showUnit }

>newtype FluxDensity a = Teslas { teslas :: a }
> deriving (Eq,Ord, Typeable, Data, Generic) deriving newtype (Binary)
>instance (Num a, Show a) => Show (FluxDensity a) where { show = showUnit }

>newtype Inductance a = Henrys { henrys :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show (Inductance a) where { show = showUnit }

>newtype LuminousFlux a = Lumens { lumens :: a }
> deriving (Eq,Ord, Typeable, Data, Generic) deriving newtype (Binary)
>instance (Num a, Show a) => Show ( LuminousFlux  a) where { show = showUnit }

>newtype Illuminance a = Luxes { luxes :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show (Illuminance a) where { show = showUnit }

>newtype Radioactivity a = Becquerels { becquerels :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show (Radioactivity a) where { show = showUnit }

>newtype AbsorbedDose a = Grays { grays :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show (AbsorbedDose a) where { show = showUnit }

>newtype EquivalentDose a = Sieverts { sieverts :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)

>instance (Num a, Show a) => Show (EquivalentDose a) where { show = showUnit }


>newtype CatalyticActivity a = Katals { katals :: a }
> deriving (Eq,Ord, Typeable, Data, Generic)
> deriving newtype (Binary)
>instance (Num a, Show a) => Show (CatalyticActivity a) where { show = showUnit }

>instance (Num a, Show a) => Unit (CatalyticActivity a) where
>   amount = katals
>   dimension _ = katalDimension
>   fromQuantity = fromQuantityDef katalDimension Katals
>   unitOf _ = "kat"
>instance (Show a, Num a) => LiteralUnit (CatalyticActivity a) where { fromAmount = Katals }
>instance (Num a, Show a) => Unit (EquivalentDose a) where
>   amount = sieverts
>   dimension _ = sievertDimension
>   fromQuantity = fromQuantityDef sievertDimension Sieverts
>   unitOf _ = "Sv"
>instance (Show a, Num a) => LiteralUnit (EquivalentDose a) where { fromAmount = Sieverts }
>   
>                                     
>instance (Num a, Show a) => Unit (AbsorbedDose a) where
>   amount = grays
>   dimension _ = grayDimension
>   fromQuantity = fromQuantityDef grayDimension Grays
>   unitOf _ = "Gy"
>instance (Show a, Num a) => LiteralUnit (AbsorbedDose a) where { fromAmount = Grays }   
>instance (Num a, Show a) => Unit (Radioactivity a) where
>   amount = becquerels
>   dimension _ = becquerelDimension
>   fromQuantity = fromQuantityDef becquerelDimension Becquerels
>   unitOf _ = "Bq"
>instance (Show a, Num a) => LiteralUnit (Radioactivity a) where { fromAmount = Becquerels }
>instance (Num a, Show a) => Unit (Illuminance a) where
>   amount = luxes
>   dimension _ = luxDimension
>   fromQuantity = fromQuantityDef luxDimension Luxes
>   unitOf _ = "lx"
>instance (Show a, Num a) => LiteralUnit (Illuminance a) where { fromAmount = Luxes }
>instance (Num a, Show a) => Unit (LuminousFlux a) where
>   amount = lumens
>   dimension _ = lumenDimension
>   fromQuantity = fromQuantityDef lumenDimension Lumens
>   unitOf _ = "lm"
>instance (Show a, Num a) => LiteralUnit (LuminousFlux a) where { fromAmount = Lumens }
>instance (Num a, Show a) => Unit (Inductance a) where
>   amount = henrys
>   dimension _ = henryDimension
>   fromQuantity = fromQuantityDef henryDimension Henrys
>   unitOf _ = "H"
>instance (Show a, Num a) => LiteralUnit (Inductance a) where { fromAmount = Henrys }
>instance (Num a, Show a) => Unit (FluxDensity a) where
>   amount = teslas
>   dimension _ = teslaDimension
>   fromQuantity = fromQuantityDef teslaDimension Teslas
>   unitOf _ = "T"
>instance (Show a, Num a) => LiteralUnit (FluxDensity a) where { fromAmount = Teslas }
>instance (Num a, Show a) => Unit (Flux a) where
>   amount = webers
>   dimension _ = weberDimension
>   fromQuantity = fromQuantityDef weberDimension Webers
>   unitOf _ = "W"
>instance (Show a, Num a) => LiteralUnit (Flux a) where { fromAmount = Webers }
>instance (Num a, Show a) => Unit (Conductance a) where
>   amount = siemenses
>   dimension _ = siemensDimension
>   fromQuantity = fromQuantityDef siemensDimension Siemenses
>   unitOf _ = "S"
>instance (Show a, Num a) => LiteralUnit (Conductance a) where { fromAmount = Siemenses }
>instance (Num a, Show a) => Unit (Resistance a) where
>   amount = ohms
>   dimension _ = ohmDimension
>   fromQuantity = fromQuantityDef ohmDimension Ohms
>   unitOf _ = "Ω"
>instance (Show a, Num a) => LiteralUnit (Resistance a) where { fromAmount = Ohms }
>instance (Num a, Show a) => Unit (Capacitance a) where
>   amount = farads
>   dimension _ = faradDimension
>   fromQuantity = fromQuantityDef faradDimension Farads
>   unitOf _ = "F"
>instance (Show a, Num a) => LiteralUnit (Capacitance a) where { fromAmount = Farads }
>instance (Num a, Show a) => Unit (Voltage a) where
>   amount = volts
>   dimension _ = voltDimension
>   fromQuantity = fromQuantityDef voltDimension Volts
>   unitOf _ = "V"
>instance (Show a, Num a) => LiteralUnit (Voltage a) where { fromAmount = Volts }
>instance (Num a, Show a) => Unit (Charge a) where
>   amount = coulombs
>   dimension _ = coulombDimension
>   fromQuantity = fromQuantityDef coulombDimension Coulombs
>   unitOf _ = "C"
>instance (Show a, Num a) => LiteralUnit (Charge a) where { fromAmount = Coulombs }
>instance (Num a, Show a) => Unit ( Power  a) where
>   amount = watts
>   dimension _ = wattDimension
>   fromQuantity = fromQuantityDef wattDimension Watts
>   unitOf _ = "W"
>instance (Show a, Num a) => LiteralUnit ( Power  a) where { fromAmount = Watts }
>instance (Num a, Show a) => Unit ( Energy  a) where
>   amount = joules
>   dimension _ = jouleDimension
>   fromQuantity = fromQuantityDef jouleDimension Joules
>   unitOf _ = "J"
>instance (Show a, Num a) => LiteralUnit ( Energy  a) where { fromAmount = Joules }
>instance (Num a, Show a) => Unit ( Pressure  a) where
>   amount = pascals
>   dimension _ = pascalDimension
>   fromQuantity = fromQuantityDef pascalDimension Pascals
>   unitOf _ = "Pa"
>instance (Show a, Num a) => LiteralUnit ( Pressure  a) where { fromAmount = Pascals }
>instance (Num a, Show a) => Unit ( Force  a) where
>   amount = newtons
>   dimension _ = newtonDimension
>   fromQuantity = fromQuantityDef newtonDimension Newtons
>   unitOf _ = "N"
>instance (Show a, Num a) => LiteralUnit ( Force  a) where { fromAmount = Newtons }
>instance (Num a, Show a) => Unit ( Torque  a) where
>   amount = newtonmeters
>   dimension _ = jouleDimension %- radianDimension
>   fromQuantity = fromQuantityDef (jouleDimension %- radianDimension) NewtonMeters
>   unitOf _ = "N m" -- notice not displayed similarly than joule.
>instance (Show a, Num a) => LiteralUnit ( Torque  a) where { fromAmount = NewtonMeters }

>instance (Num a, Show a) => Unit ( Frequency  a) where
>   amount = hertzes
>   dimension _ = hertzDimension
>   fromQuantity = fromQuantityDef hertzDimension Hertzes
>   unitOf _ = "Hz"
>instance (Show a, Num a) => LiteralUnit ( Frequency  a) where { fromAmount = Hertzes }
>instance (Num a, Show a) => Unit ( Length  a) where
>   amount = meters
>   dimension _ = meterDimension
>   fromQuantity = fromQuantityDef meterDimension Meters
>   unitOf _ = "m"
>instance (Show a, Num a) => LiteralUnit ( Length  a) where { fromAmount = Meters }
>instance (Num a, Show a) => Unit ( Mass  a) where
>   amount = kilograms
>   dimension _ = kilogramDimension
>   fromQuantity = fromQuantityDef kilogramDimension Kilograms
>   unitOf _ = "kg"
>instance (Show a, Num a) => LiteralUnit ( Mass  a) where { fromAmount = Kilograms }
>instance (Num a, Show a) => Unit ( Time  a) where
>   amount = seconds
>   dimension _ = secondDimension
>   fromQuantity = fromQuantityDef secondDimension Seconds
>   unitOf _ = "s"
>instance (Show a, Num a) => LiteralUnit ( Time  a) where { fromAmount = Seconds }
>instance (Num a, Show a) => Unit ( Current  a) where
>   amount = amperes
>   dimension _ = ampereDimension
>   fromQuantity = fromQuantityDef ampereDimension Amperes
>   unitOf _ = "A"
>instance (Show a, Num a) => LiteralUnit ( Current  a) where { fromAmount = Amperes }
>instance (Num a, Show a) => Unit ( Temperature  a) where
>   amount = kelvins
>   dimension _ = kelvinDimension
>   fromQuantity = fromQuantityDef kelvinDimension DegreesKelvin
>   unitOf _ = "K"
>instance (Show a, Num a) => LiteralUnit ( Temperature  a) where { fromAmount = DegreesKelvin }
> 
>instance (Num a, Show a) => Unit ( Substance  a) where
>   amount = moles
>   dimension _ = molDimension
>   fromQuantity = fromQuantityDef molDimension Moles
>   unitOf _ = "mol"
>instance (Show a, Num a) => LiteralUnit ( Substance  a) where { fromAmount = Moles }
>instance (Num a, Show a) => Unit ( Intensity  a) where
>   amount = candelas
>   dimension _ = candelaDimension
>   fromQuantity = fromQuantityDef candelaDimension Candelas
>   unitOf _ = "cd"
>instance (Show a, Num a) => LiteralUnit ( Intensity  a) where { fromAmount = Candelas }
>instance (Floating a, Read a, Show a) => Read (Angle a) where { readPrec = readUnit fromAmount }
>instance (Show a, Read a, Num a) => Read (Resistance a) where { readPrec = readUnit fromAmount }
>instance (Floating a, Read a, Show a) => Read (DegreesAngle a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Information  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Inductance  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( EquivalentDose  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Conductance  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( CatalyticActivity  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Radioactivity  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Illuminance  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( AbsorbedDose  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( FluxDensity  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Flux  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( LuminousFlux  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Capacitance  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Charge  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Power  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Voltage  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Energy  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Pressure  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Force  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Intensity  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Substance  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Temperature  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Frequency  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( SolidAngle  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Fractional a, Read a) => Read ( DegreesTemperature  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Current  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Fractional a, Read a) => Read ( DegreesFahrenheit  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Fractional a, Read a) => Read ( DegreesRankine  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Fractional a, Read a) => Read ( Percentage  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Time  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Length  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Mass  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( SquareLength  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( CubicLength  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Acceleration  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Velocity  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Num a, Read a) => Read ( Torque  a) where { readPrec = readUnit fromAmount }
>instance (Show a, Floating a, Read a) => Read ( SoundLevel  a) where { readPrec = readUnit fromAmount }
>instance (LiteralUnit a, LiteralUnit b, Read (Scalar a), Show (Scalar a), Scalar a ~ Scalar b) => Read (a :* b) where { readPrec = readUnit (fromAmount *%% fromAmount) }
>instance (LiteralUnit a, LiteralUnit b, Read (Scalar a), Show (Scalar a), Scalar a ~ Scalar b) => Read (a :/ b) where { readPrec = readUnit (fromAmount /%% fromAmount) }

>instance (Num a) => VectorSpace (Angle a) where
>  type Scalar (Angle a) = a
>  vzero = Radians 0
>  vnegate (Radians a) = Radians (negate a)
>  (Radians a) %+ (Radians b) = Radians (a + b)
>  k %* (Radians a) = Radians $ k * a

>instance (Num a) => VectorSpace (DegreesAngle a) where
>  type Scalar (DegreesAngle a) = a
>  vzero = Degrees 0
>  vnegate (Degrees a) = Degrees (negate a)
>  (Degrees a) %+ (Degrees b) = Degrees (a + b)
>  k %* (Degrees a) = Degrees $ k * a

#if __GLASGOW_HASKELL__ >= 806

>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Resistance a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Information a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Inductance a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (EquivalentDose a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Conductance a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (CatalyticActivity a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Radioactivity a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Illuminance a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (AbsorbedDose a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (FluxDensity a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Flux a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (LuminousFlux a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Capacitance a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Charge a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Power a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Voltage a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Energy a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Pressure a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Force a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Intensity a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Substance a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Temperature a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Frequency a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (SolidAngle a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (DegreesTemperature a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Current a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (DegreesFahrenheit a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (DegreesRankine a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Percentage a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Time a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Length a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Mass a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (SquareLength a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (CubicLength a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Acceleration a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Velocity a)
>deriving via (Dimensionless a) instance (Num a) => VectorSpace (Torque a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Angle a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Resistance a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (DegreesAngle a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Information a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Inductance a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (EquivalentDose a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Conductance a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (CatalyticActivity a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Radioactivity a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Illuminance a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (AbsorbedDose a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (FluxDensity a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Flux a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (LuminousFlux a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Capacitance a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Charge a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Power a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Voltage a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Energy a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Pressure a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Force a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Intensity a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Substance a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Temperature a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Frequency a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (SolidAngle a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (DegreesTemperature a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Current a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (DegreesFahrenheit a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (DegreesRankine a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Percentage a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Time a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Length a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Mass a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (SquareLength a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Acceleration a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Velocity a)
>deriving via (Dimensionless a) instance (Num a) => NormedSpace (Torque a)

#else

>instance (Num a) => VectorSpace ( Velocity  a) where
>   type Scalar (Velocity a) = a
>   vzero = MetersPerSecond 0
>   vnegate (MetersPerSecond x) = MetersPerSecond (negate x)
>   (MetersPerSecond x) %+ (MetersPerSecond y) = MetersPerSecond $ x + y
>   k %* (MetersPerSecond x) = MetersPerSecond (k * x)
>instance NormedSpace Velocity where { norm = amount }
>instance (Num a) => VectorSpace ( Acceleration  a) where
>   type Scalar (Acceleration a) = a
>   vzero = MetersPerSquareSecond 0
>   vnegate (MetersPerSquareSecond x) = MetersPerSquareSecond (negate x)
>   (MetersPerSquareSecond x) %+ (MetersPerSquareSecond y) = MetersPerSquareSecond (x + y)
>   k %* (MetersPerSquareSecond x) = MetersPerSquareSecond (k %* x)
>instance NormedSpace Acceleration where { norm = amount }
>instance (Num a) => VectorSpace ( CubicLength  a) where
>   type Scalar (CubicLength a) = a
>   vzero = CubicMeters 0
>   vnegate (CubicMeters x) = CubicMeters $ negate x
>   (CubicMeters x) %+ (CubicMeters y) = CubicMeters $ x + y
>   k %* (CubicMeters x) = CubicMeters $ k * x
>instance (Num a) => VectorSpace ( SquareLength  a) where
>   type Scalar (SquareLength a) = a
>   vzero = SquareMeters 0
>   vnegate (SquareMeters x) = SquareMeters $ negate x
>   (SquareMeters x) %+ (SquareMeters y) = SquareMeters $ x + y
>   k %* (SquareMeters x) = SquareMeters $ k * x
>instance NormedSpace SquareLength where { norm = amount }

>instance (Num a) => VectorSpace ( Mass  a) where
>   type Scalar (Mass a) = a
>   vzero = Kilograms 0
>   vnegate (Kilograms x) = Kilograms $ negate x
>   (Kilograms x) %+ (Kilograms y) = Kilograms $ x + y
>   k %* (Kilograms x) = Kilograms $ k * x
>instance NormedSpace Mass where { norm = amount }
>instance (Num a) => VectorSpace ( Length  a) where
>   type Scalar (Length a) = a
>   vzero = Meters 0
>   vnegate (Meters x) = Meters $ negate x
>   (Meters x) %+ (Meters y) = Meters $ x + y
>   k %* (Meters x) = Meters $ k * x
>instance NormedSpace Length where { norm = amount }
>instance (Num a) => VectorSpace ( Time  a) where
>   type Scalar (Time a) = a
>   vzero = Seconds 0
>   vnegate (Seconds x) = Seconds $ negate x
>   (Seconds x) %+ (Seconds y) = Seconds $ x + y
>   k %* (Seconds x) = Seconds $ k * x
>instance NormedSpace Time where { norm = amount }
>instance (Num a) => VectorSpace ( Percentage  a) where
>   type Scalar (Percentage a) = a
>   vzero = Percentages 0
>   vnegate (Percentages i) = Percentages $ negate i
>   (Percentages x) %+ (Percentages y) = Percentages $ x + y
>   k %* (Percentages x) = Percentages (k * x)
>instance NormedSpace Percentage where { norm = amount }
>instance (Num a) => VectorSpace ( DegreesFahrenheit  a) where
>   type Scalar (DegreesFahrenheit a) = a
>   vzero = DegreesFahrenheit 0
>   vnegate (DegreesFahrenheit x) = DegreesFahrenheit $ negate x
>   (DegreesFahrenheit x) %+ (DegreesFahrenheit y) = DegreesFahrenheit $ x + y
>   k %* (DegreesFahrenheit x) = DegreesFahrenheit $ k * x
>instance NormedSpace DegreesFahrenheit where { norm = amount }
>instance (Num a) => VectorSpace ( DegreesRankine  a) where
>   type Scalar (DegreesRankine a) = a
>   vzero = DegreesRankine 0
>   vnegate (DegreesRankine x) = DegreesRankine $ negate x
>   (DegreesRankine x) %+ (DegreesRankine y) = DegreesRankine $ x + y
>   k %* (DegreesRankine x) = DegreesRankine $ k * x
>instance NormedSpace DegreesRankine where { norm = amount }
>instance (Num a) => VectorSpace ( Current  a) where
>   type Scalar (Current a) = a
>   vzero = Amperes 0
>   vnegate (Amperes x) = Amperes $ negate x
>   (Amperes x) %+ (Amperes y) = Amperes $ x + y
>   k %* (Amperes x) = Amperes $ k * x
>instance NormedSpace Current where { norm = amount }
>instance (Num a) => VectorSpace ( DegreesTemperature  a) where
>   type Scalar (DegreesTemperature a) = a
>   vzero = DegreesCelcius 0
>   vnegate (DegreesCelcius x) = DegreesCelcius $ negate x
>   (DegreesCelcius x) %+ (DegreesCelcius y) = DegreesCelcius $ x + y
>   k %* (DegreesCelcius x) = DegreesCelcius $ k * x
>instance NormedSpace DegreesTemperature where { norm = amount }
>instance (Num a) => VectorSpace ( SolidAngle  a) where
>   type Scalar (SolidAngle a) = a
>   vzero = Steradians 0
>   vnegate (Steradians x) = Steradians (negate x)
>   (Steradians x) %+ (Steradians y) = Steradians $ x + y
>   k %* (Steradians x) = Steradians $ k * x
>instance NormedSpace SolidAngle where { norm = amount }
>instance (Num a) => VectorSpace ( Frequency  a) where
>   type Scalar (Frequency a) = a
>   vzero = Hertzes 0
>   vnegate (Hertzes x) = Hertzes $ negate x
>   (Hertzes x) %+ (Hertzes y) = Hertzes $ x + y
>   k %* (Hertzes x) = Hertzes $ k * x
>instance NormedSpace Frequency where { norm = amount }
>instance (Num a) => VectorSpace ( Temperature  a) where
>   type Scalar (Temperature a) = a
>   vzero = DegreesKelvin 0
>   vnegate (DegreesKelvin x) = DegreesKelvin $ negate x
>   (DegreesKelvin x) %+ (DegreesKelvin y) = DegreesKelvin $ x + y
>   k %* (DegreesKelvin x) = DegreesKelvin $ k * x
>instance NormedSpace Temperature where { norm = amount }
>instance (Num a) => VectorSpace ( Substance  a) where
>   type Scalar (Substance a) = a
>   vzero = Moles 0
>   vnegate (Moles x) = Moles $ negate x
>   (Moles x) %+ (Moles y) = Moles $ x + y
>   k %* (Moles x) = Moles $ k * x
>instance NormedSpace Substance where { norm = amount }
>instance (Num a) => VectorSpace ( Intensity  a) where
>   type Scalar (Intensity a) = a
>   vzero = Candelas 0
>   vnegate (Candelas x) = Candelas $ negate x
>   (Candelas x) %+ (Candelas y) = Candelas $ x + y
>   k %* (Candelas x) = Candelas $ k * x
>instance NormedSpace Intensity where { norm = amount }
>instance (Num a) => VectorSpace ( Force  a) where
>   type Scalar (Force a) = a
>   vzero = Newtons 0
>   vnegate (Newtons x) = Newtons $ negate x
>   (Newtons x) %+ (Newtons y) = Newtons $ x + y
>   k %* (Newtons x) = Newtons $ k * x
>instance NormedSpace Force where { norm = amount }
>instance (Num a) => VectorSpace ( Pressure  a) where
>   type Scalar (Pressure a) = a
>   vzero = Pascals 0
>   vnegate (Pascals x) = Pascals $ negate x
>   (Pascals x) %+ (Pascals y) = Pascals $ x + y
>   k %* (Pascals x) = Pascals $ k * x
>instance NormedSpace Pressure where { norm = amount }
>instance (Num a) => VectorSpace ( Energy  a) where
>   type Scalar (Energy a) = a
>   vzero = Joules 0
>   vnegate (Joules x) = Joules $ negate x
>   (Joules x) %+ (Joules y) = Joules $ x + y
>   k %* (Joules x) = Joules $ k * x
>instance NormedSpace Energy where { norm = amount }
>instance (Num a) => VectorSpace ( Voltage  a) where
>   type Scalar (Voltage a) = a
>   vzero = Volts 0
>   vnegate (Volts x) = Volts $ negate x
>   (Volts x) %+ (Volts y) = Volts $ x + y
>   k %* (Volts x) = Volts $ k * x
>instance NormedSpace Voltage where { norm = amount }
>instance (Num a) => VectorSpace ( Power  a) where
>   type Scalar (Power a) = a
>   vzero = Watts 0
>   vnegate (Watts x) = Watts $ negate x
>   (Watts x) %+ (Watts y) = Watts $ x + y
>   k %* (Watts x) = Watts $ k * x
>instance NormedSpace Power where { norm = amount }
>instance (Num a) => VectorSpace ( Charge  a) where
>   type Scalar (Charge a) = a
>   vzero = Coulombs 0
>   vnegate (Coulombs x) = Coulombs $ negate x
>   (Coulombs x) %+ (Coulombs y) = Coulombs $ x + y
>   k %* (Coulombs x) = Coulombs $ k * x
>instance NormedSpace Charge where { norm = amount }
>instance (Num a) => VectorSpace ( Capacitance  a) where
>   type Scalar (Capacitance a) = a
>   vzero = Farads 0
>   vnegate (Farads x) = Farads $ negate x
>   (Farads x) %+ (Farads y) = Farads $ x + y
>   k %* (Farads x) = Farads $ k * x
>instance NormedSpace Capacitance where { norm = amount }
>instance (Num a) => VectorSpace ( LuminousFlux  a) where
>   type Scalar (LuminousFlux a) = a
>   vzero = Lumens 0
>   vnegate (Lumens x) = Lumens $ negate x
>   (Lumens x) %+ (Lumens y) = Lumens $ x + y
>   k %* (Lumens x) = Lumens $ k * x
>instance NormedSpace LuminousFlux where { norm = amount }
>instance (Num a) => VectorSpace ( Flux  a) where
>   type Scalar (Flux a) = a
>   vzero = Webers 0
>   vnegate (Webers x) = Webers $ negate x
>   (Webers x) %+ (Webers y) = Webers $ x + y
>   k %* (Webers x) = Webers $ k * x
>instance NormedSpace Flux where { norm = amount }

>instance (Num a) => VectorSpace ( FluxDensity  a) where
>   type Scalar (FluxDensity a) = a
>   vzero = Teslas 0
>   vnegate (Teslas x) = Teslas $ negate x
>   (Teslas x) %+ (Teslas y) = Teslas $ x + y
>   k %* (Teslas x) = Teslas $ k * x
>instance NormedSpace FluxDensity where { norm = amount }
>instance (Num a) => VectorSpace ( AbsorbedDose  a) where
>   type Scalar (AbsorbedDose a) = a
>   vzero = Grays 0
>   vnegate (Grays x) = Grays $ negate x
>   (Grays x) %+ (Grays y) = Grays $ x + y
>   k %* (Grays x) = Grays $ k * x
>instance NormedSpace AbsorbedDose where { norm = amount }
>instance (Num a) => VectorSpace ( Illuminance  a) where
>   type Scalar (Illuminance a) = a
>   vzero = Luxes 0
>   vnegate (Luxes x) = Luxes $ negate x
>   (Luxes x) %+ (Luxes y) = Luxes $ x + y
>   k %* (Luxes x) = Luxes $ k * x
>instance NormedSpace Illuminance where { norm = amount }
>instance (Num a) => VectorSpace ( Radioactivity  a) where
>   type Scalar (Radioactivity a) = a
>   vzero = Becquerels 0
>   vnegate (Becquerels x) = Becquerels $ negate x
>   (Becquerels x) %+ (Becquerels y) = Becquerels $ x + y
>   k %* (Becquerels x) = Becquerels $ k * x
>instance NormedSpace Radioactivity where { norm = amount }
>instance (Num a) => VectorSpace ( CatalyticActivity  a) where
>   type Scalar (CatalyticActivity a) = a
>   vzero = Katals 0
>   vnegate (Katals x) = Katals $ negate x
>   (Katals x) %+ (Katals y) = Katals $ x + y
>   k %* (Katals x) = Katals $ k * x
>instance NormedSpace CatalyticActivity where { norm = amount }
>instance (Num a) => VectorSpace ( Conductance  a) where
>   type Scalar (Conductance a) = a
>   vzero = Siemenses 0
>   vnegate (Siemenses x) = Siemenses $ negate x
>   (Siemenses x) %+ (Siemenses y) = Siemenses $ x + y
>   k %* (Siemenses x) = Siemenses $ k * x
>instance NormedSpace Conductance where { norm = amount }

>instance (Num a) => VectorSpace ( EquivalentDose  a) where
>   type Scalar (EquivalentDose a) = a
>   vzero = Sieverts 0
>   vnegate (Sieverts x) = Sieverts $ negate x
>   (Sieverts x) %+ (Sieverts y) = Sieverts $ x + y
>   k %* (Sieverts x) = Sieverts $ k * x
>instance NormedSpace EquivalentDose where { norm = amount }

>instance (Num a) => VectorSpace ( Angle  a) where
>   type Scalar (Angle a) = a
>   vzero = Radians 0
>   vnegate (Radians x) = Radians (negate x)
>   (Radians x) %+ (Radians y) = Radians $ x + y
>   k %* (Radians x) = Radians $ k * x

>instance NormedSpace Angle where { norm = amount }

>instance (Num a) => VectorSpace ( Resistance  a) where
>   type Scalar (Resistance a) = a
>   vzero = Ohms 0
>   vnegate (Ohms x) = Ohms $ negate x
>   (Ohms x) %+ (Ohms y) = Ohms $ x + y
>   k %* (Ohms x) = Ohms $ k * x
>instance NormedSpace Resistance where { norm = amount }

>instance (Num a) => VectorSpace ( DegreesAngle  a) where
>   type Scalar (DegreesAngle a) = a
>   vzero = Degrees 0
>   vnegate (Degrees x) = Degrees $ negate x
>   (Degrees x) %+ (Degrees y) = Degrees $ x + y
>   k %* (Degrees x) = Degrees $ k * x
>instance NormedSpace DegreesAngle where { norm = amount }

>instance (Num a) => VectorSpace ( Information  a) where
>   type Scalar (Information a) = a
>   vzero = Bits 0
>   vnegate (Bits x) = Bits (negate x)
>   (Bits x) %+ (Bits y) = Bits $ x + y
>   k %* (Bits x) = Bits (k * x)
>instance NormedSpace Information where { norm = amount }

>instance (Num a) => VectorSpace ( Inductance  a) where
>   type Scalar (Inductance a) = a
>   vzero = Henrys 0
>   vnegate (Henrys x) = Henrys $ negate x
>   (Henrys x) %+ (Henrys y) = Henrys $ x + y
>   k %* (Henrys x) = Henrys $ k * x
>instance NormedSpace Inductance where { norm = amount }

>instance (Num a) => VectorSpace ( Torque  a) where
>   type Scalar (Torque a) = a
>   vzero = NewtonMeters 0
>   vnegate (NewtonMeters x) = NewtonMeters $ negate x
>   (NewtonMeters x) %+ (NewtonMeters y) = NewtonMeters $ x + y
>   k %* (NewtonMeters x) = NewtonMeters $ k * x
>instance NormedSpace Torque where { norm = amount }

#endif
