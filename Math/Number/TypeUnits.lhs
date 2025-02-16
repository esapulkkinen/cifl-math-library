>{-# LANGUAGE Trustworthy, DataKinds, TypeOperators, MultiParamTypeClasses, KindSignatures #-}
>{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, GADTs, DataKinds, PolyKinds, StandaloneDeriving #-}
>{-# LANGUAGE FlexibleContexts, DerivingStrategies, GeneralizedNewtypeDeriving, TypeFamilyDependencies #-}
>{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveDataTypeable #-}
>{-# LANGUAGE UndecidableInstances #-}
>module Math.Number.TypeUnits where
>import Data.Kind
>import Math.Matrix.Interface
>import Math.Number.DimensionalAnalysis
>import Math.Number.Units
>import Math.Number.TypeRational
>import Data.Typeable
>import Data.Data
>import GHC.Generics
>import GHC.Types
>import GHC.TypeLits hiding (SNat)

>data DDimension = DDimension {
>   dlength_power :: SNat, -- meters
>   dweight_power :: SNat, -- kilograms
>   dtime_power :: SNat,   -- seconds
>   dcurrent_power :: SNat, -- amperes
>   dtemperature_power :: SNat, -- kelvins
>   dluminosity_power :: SNat,  -- candelas
>   dsubstance_power :: SNat, -- moles
>   dcircular_power :: SNat -- radians
> }
>
>
> 
>type DDimensionless = 'DDimension 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze
>type DLength = 'DDimension SOne 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze
>type DWeight = 'DDimension 'Ze SOne 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze
>type DTime   = 'DDimension 'Ze 'Ze SOne 'Ze 'Ze 'Ze 'Ze 'Ze
>type DCurrent = 'DDimension 'Ze 'Ze 'Ze SOne 'Ze 'Ze 'Ze 'Ze
>type DTemperature = 'DDimension 'Ze 'Ze 'Ze 'Ze SOne 'Ze 'Ze 'Ze
>type DLuminosity = 'DDimension 'Ze 'Ze 'Ze 'Ze 'Ze SOne 'Ze 'Ze
>type DSubstance = 'DDimension 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze SOne 'Ze
>type DAngle     = 'DDimension 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze SOne
>type DAngularSpeed = 'DDimension 'Ze 'Ze ('SNegate SOne) 'Ze 'Ze 'Ze 'Ze SOne
>type DAngularAcceleration = 'DDimension 'Ze 'Ze ('SNegate STwo) 'Ze 'Ze 'Ze 'Ze SOne
>type DSolidAngle = 'DDimension 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze STwo
>type DSquareLength = 'DDimension STwo 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze
>type DCubicLength = 'DDimension SThree 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze
>type DVelocity = 'DDimension SOne 'Ze ('SNegate SOne) 'Ze 'Ze 'Ze 'Ze 'Ze
>type DAcceleration = 'DDimension SOne 'Ze ('SNegate STwo) 'Ze 'Ze 'Ze 'Ze 'Ze
>type DMass = 'DDimension 'Ze SOne 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze
>type DFrequency = 'DDimension 'Ze 'Ze ('SNegate SOne) 'Ze 'Ze 'Ze 'Ze 'Ze
>type DForce = 'DDimension SOne SOne ('SNegate STwo) 'Ze 'Ze 'Ze 'Ze 'Ze
>type DInductance = 'DDimension STwo SOne ('SNegate STwo) ('SNegate STwo) 'Ze 'Ze 'Ze 'Ze
>-- | <https://en.wikipedia.org/wiki/Torque>
>type DTorque = 'DDimension STwo SOne ('SNegate STwo) 'Ze 'Ze 'Ze 'Ze ('SNegate SOne)
>type DPressure = 'DDimension ('SNegate SOne) SOne ('SNegate STwo) 'Ze 'Ze 'Ze 'Ze 'Ze
>type DEnergy = 'DDimension STwo SOne ('SNegate STwo) 'Ze 'Ze 'Ze 'Ze 'Ze
>type DPower = 'DDimension STwo SOne ('SNegate SThree) 'Ze 'Ze 'Ze 'Ze 'Ze
>type DCharge = 'DDimension 'Ze 'Ze SOne SOne 'Ze 'Ze 'Ze 'Ze
>type DVoltage = 'DDimension STwo SOne ('SNegate SThree) ('SNegate SOne) 'Ze 'Ze 'Ze 'Ze
>type DCapacitance = 'DDimension ('SNegate STwo) ('SNegate SOne) SFour STwo 'Ze 'Ze 'Ze 'Ze
>type DResistance = 'DDimension STwo SOne ('SNegate SThree) ('SNegate STwo) 'Ze 'Ze 'Ze 'Ze
>type DConductance = 'DDimension ('SNegate STwo) ('SNegate SOne) SThree STwo 'Ze 'Ze 'Ze 'Ze
>type DFlux = 'DDimension STwo SOne ('SNegate STwo) ('SNegate SOne) 'Ze 'Ze 'Ze 'Ze
>type DFluxDensity = 'DDimension 'Ze SOne ('SNegate STwo) ('SNegate SOne) 'Ze 'Ze 'Ze 'Ze
>-- | <https://en.wikipedia.org/wiki/Luminous_flux>
>type DLuminousEnergy = 'DDimension 'Ze 'Ze SOne 'Ze 'Ze SOne 'Ze STwo
>type DLuminousIntensity = 'DDimension 'Ze 'Ze 'Ze 'Ze 'Ze SOne 'Ze 'Ze
>type DLuminousFlux = 'DDimension 'Ze 'Ze 'Ze 'Ze 'Ze SOne 'Ze STwo
>type DLuminousExitance = 'DDimension ('SNegate STwo) 'Ze 'Ze 'Ze 'Ze SOne 'Ze STwo
>type DIlluminance = 'DDimension ('SNegate STwo) 'Ze 'Ze 'Ze 'Ze SOne 'Ze STwo
>type DLuminance = 'DDimension ('SNegate STwo) 'Ze 'Ze 'Ze 'Ze SOne 'Ze 'Ze
>type DRadioactivity = 'DDimension 'Ze ('SNegate SOne) 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze
>type DAbsorbedDose = 'DDimension STwo ('SNegate STwo) 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze
>type DEquivalentDose = 'DDimension ('SNegate STwo) ('SNegate SOne) SThree STwo 'Ze 'Ze 'Ze 'Ze
>type DCatalyticActivity = 'DDimension 'Ze ('SNegate SOne) 'Ze 'Ze 'Ze 'Ze SOne 'Ze

>type family DimensionPower (a :: DDimension) (n :: Nat) :: DDimension
>type instance DimensionPower a 0 = DDimensionless
>type instance DimensionPower a 1 = a
>type instance DimensionPower a 2 = DimensionPlus a a
>type instance DimensionPower a 3 = DimensionPlus a (DimensionPower a 2)
>type instance DimensionPower a 4 = DimensionPlus a (DimensionPower a 3)
>type instance DimensionPower a 5 = DimensionPlus a (DimensionPower a 4)
>type instance DimensionPower a 6 = DimensionPlus a (DimensionPower a 5)
>type instance DimensionPower a 7 = DimensionPlus a (DimensionPower a 6)

>type family DimensionNegativePower (a :: DDimension) (n :: Nat) :: DDimension
>type instance DimensionNegativePower a n = DimensionInverse (DimensionPower a n)

>type family DimensionInverse (a :: DDimension) :: DDimension
>type instance DimensionInverse a = DimensionMinus DDimensionless a

>type family DimensionPlus (a :: DDimension) (b :: DDimension) :: DDimension
>type instance DimensionPlus ('DDimension a1 a2 a3 a4 a5 a6 a7 a8) ('DDimension b1 b2 b3 b4 b5 b6 b7 b8) =
>       'DDimension (SPlus a1 b1) (SPlus a2 b2) (SPlus a3 b3) (SPlus a4 b4) (SPlus a5 b5) (SPlus a6 b6) (SPlus a7 b7) (SPlus a8 b8)
>type family DimensionMinus (a :: DDimension) (b :: DDimension) :: DDimension
>type instance DimensionMinus ('DDimension a1 a2 a3 a4 a5 a6 a7 a8) ('DDimension b1 b2 b3 b4 b5 b6 b7 b8) =
>       'DDimension (SMinus a1 b1) (SMinus a2 b2) (SMinus a3 b3) (SMinus a4 b4) (SMinus a5 b5) (SMinus a6 b6) (SMinus a7 b7) (SMinus a8 b8)
>
>unit_multiply :: (
> Scalar (DUnit (DimensionPlus u u') a) ~ Scalar (DUnit u' a),
> Scalar (DUnit u a) ~ Scalar (DUnit u' a),
> Unit (DUnit u a), Unit (DUnit u' a), LiteralUnit (DUnit (DimensionPlus u u') a))
>  => DUnit u a -> DUnit u' a -> DUnit (DimensionPlus u u') a
>unit_multiply a b = fromAmount (amount a * amount b) 

>unit_divide :: (
>    Fractional (Scalar (DUnit u' a)),
>    Scalar (DUnit (DimensionMinus u u') a) ~ Scalar (DUnit u' a),
>    Scalar (DUnit u a) ~ Scalar (DUnit u' a),
>     Unit (DUnit u a), Unit (DUnit u' a), LiteralUnit (DUnit (DimensionMinus u u') a))
>  => DUnit u a -> DUnit u' a -> DUnit (DimensionMinus u u') a
>unit_divide a b = fromAmount (amount a / amount b)

>half_circle_angle :: (Floating a) => DUnit DAngle a
>half_circle_angle = DAngle $ Radians pi

>full_circle_angle :: (Floating a) => DUnit DAngle a
>full_circle_angle = DAngle $ Radians (2 * pi)
>
>type family (:#:) a (dim :: [DDimension])
>type instance (:#:) s '[] = DUnit DDimensionless s
>type instance (:#:) s (a ': '[]) = DUnit a s
>type instance (:#:) d (a ': b ': cr) = d :#: (DimensionPlus a b ': cr)
>
>type family (:/:) a (dim :: [DDimension])
>type instance (:/:) a '[] = a
>type instance (:/:) (DUnit a s) (b ': '[]) = DUnit (DimensionMinus a b) s
>type instance (:/:) a (b ': c ': cr) = a :/: (DimensionPlus b c ': cr)
>
>type family (:++) (a :: [k]) (b :: [k]) :: [k]
>type instance (:++) '[] b = b
>type instance (:++) (a ': ar) b = a ': (ar :++ b)

(*%%%) :: (Scalar (Double :#: lst) ~ Scalar (Double :#: lst')
       , Scalar (Double :#: lst) ~ Scalar (Double :#: (lst :++ lst'))
       , Unit (Double :#: lst), Unit (Double :#: lst')
       , Unit (Double :#: (lst :++ lst'))
       , LiteralUnit (Double :#: (lst :++ lst'))
 )
  => Double :#: lst -> Double :#: lst' -> Double :#: (lst :++ lst')
a *%%% b = fromAmount (amount a * amount b)
 
>data family DUnit (u :: DDimension) a
>data instance DUnit DAngle a = DAngle (Angle a)
>                           | DDegreesAngle (DegreesAngle a)
>                           | DAngleZero
>   deriving (Show, Read,Eq)

>newtype instance DUnit DSolidAngle a = DSolidAngle (SolidAngle a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)

>-- | DDimensionlessZero is needed to avoid type error in vzero.
>data instance DUnit DDimensionless a = DDimensionless (Dimensionless a)
>                                   | DInformation (Information a)
>                                   | DSoundLevel (SoundLevel a)
>                                   | DPercentage (Percentage a)
>                                   | DDimensionlessZero
>   deriving (Show, Read,Eq, Typeable, Data, Generic)
>instance (Num a) => VectorSpace (DUnit DAngle a) where
>   type Scalar (DUnit DAngle a) = a
>   vzero = DAngleZero
>   vnegate (DAngle a) = DAngle (vnegate a)
>   vnegate (DDegreesAngle a) = DDegreesAngle (vnegate a)
>   (DAngle a) %+ (DAngle b) = DAngle (a %+ b)
>   (DDegreesAngle a) %+ (DDegreesAngle b) = DDegreesAngle (a %+ b)
>   DAngleZero %+ a = a
>   a %+ DAngleZero = a
>   _ %+ _ = error "ill-typed DUnit DAngle addition"
>   k %* (DAngle a) = DAngle (k %* a)
>   k %* (DDegreesAngle a) = DDegreesAngle (k %* a)

>instance (Floating a) => VectorSpace (DUnit DDimensionless a) where
>   type Scalar (DUnit DDimensionless a) = Scalar (Dimensionless a)
>   vzero = DDimensionlessZero
>   vnegate (DDimensionless a) = DDimensionless (vnegate a)
>   vnegate (DInformation a) = DInformation (vnegate a)
>   vnegate (DSoundLevel a) = DSoundLevel (vnegate a)
>   vnegate (DPercentage a) = DPercentage (vnegate a)
>   vnegate DDimensionlessZero = DDimensionlessZero
>   (DDimensionless a) %+ (DDimensionless b) = DDimensionless (a %+ b)
>   (DInformation a) %+ (DInformation b) = DInformation (a %+ b)
>   (DSoundLevel a) %+ (DSoundLevel b) = DSoundLevel (a %+ b)
>   (DPercentage a) %+ (DPercentage b) = DPercentage (a %+ b)
>   DDimensionlessZero %+ a = a
>   a %+ DDimensionlessZero = a
>   _ %+ _ = error "ill-typed DUnit DDimensionless addition"
>   k %* (DDimensionless a) = DDimensionless (k %* a)
>   k %* (DInformation a) = DInformation (k %* a)
>   k %* (DSoundLevel a) = DSoundLevel (k %* a)
>   k %* (DPercentage a) = DPercentage (k %* a)
>   k %* DDimensionlessZero = DDimensionlessZero
>newtype instance DUnit DLength a = DLength (Length a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DSquareLength a = DSquareLength (SquareLength a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DCubicLength a = DCubicLength (CubicLength a)
>   deriving (Show, Read, Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DMass a = DMass (Mass a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DTime a = DTime (Time a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DCurrent a = DCurrent (Current a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DVelocity a = DVelocity (Velocity a)
>   deriving (Show, Read, Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DAcceleration a = DAcceleration (Acceleration a)
>   deriving (Show, Read, Eq)
>   deriving newtype (VectorSpace, LiteralUnit)
>data instance DUnit DTemperature a = DDegreesFahrenheit (DegreesFahrenheit a)
>                                 | DDegreesCelcius (DegreesTemperature a)
>                                 | DDegreesKelvin (Temperature a)
>                                 | DZeroTemperature
>   deriving (Show, Read,Eq)
>instance (Num a) => VectorSpace (DUnit DTemperature a) where
>   type Scalar (DUnit DTemperature a) = a
>   vzero = DZeroTemperature
>   vnegate (DDegreesFahrenheit a) = DDegreesFahrenheit (vnegate a)
>   vnegate (DDegreesCelcius a) = DDegreesCelcius (vnegate a)
>   vnegate (DDegreesKelvin a) = DDegreesKelvin (vnegate a)
>   vnegate DZeroTemperature = DZeroTemperature
>   (DDegreesFahrenheit a) %+ (DDegreesFahrenheit b) = DDegreesFahrenheit (a %+ b)
>   (DDegreesCelcius a) %+ (DDegreesCelcius b) = DDegreesCelcius (a %+ b)
>   (DDegreesKelvin a) %+ (DDegreesKelvin b) = DDegreesKelvin (a %+ b)
>   DZeroTemperature %+ a = a
>   a %+ DZeroTemperature = a
>   _ %+ _ = error "ill-typed temperature addition"
>   k %* (DDegreesFahrenheit a) = DDegreesFahrenheit (k %* a)
>   k %* (DDegreesCelcius a) = DDegreesCelcius (k %* a)
>   k %* (DDegreesKelvin a) = DDegreesKelvin (k %* a)
>   k %* DZeroTemperature = DZeroTemperature
>newtype instance DUnit DSubstance a = DSubstance (Substance a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>data instance DUnit DLuminosity a = DLuminosity (Intensity a)
>                                | DLuminousFlux (LuminousFlux a)
>                                | DZeroLuminosity
>   deriving (Show, Read,Eq)
>instance (Num a) => VectorSpace (DUnit DLuminosity a) where
>   type Scalar (DUnit DLuminosity a) = a
>   vzero = DZeroLuminosity
>   vnegate (DLuminosity a) = DLuminosity (vnegate a)
>   vnegate (DLuminousFlux a) = DLuminousFlux (vnegate a)
>   vnegate DZeroLuminosity = DZeroLuminosity
>   (DLuminosity a) %+ (DLuminosity b) = DLuminosity (a %+ b)
>   (DLuminousFlux a) %+ (DLuminousFlux b) = DLuminousFlux (a %+ b)
>   DZeroLuminosity %+ a = a
>   a %+ DZeroLuminosity = a
>   _ %+ _ = error "ill-typed DUnit DLuminosity addition"
>   k %* (DLuminosity a) = DLuminosity (k %* a)
>   k %* (DLuminousFlux a) = DLuminousFlux (k %* a)
>   k %* DZeroLuminosity = DZeroLuminosity
>newtype instance DUnit DFrequency a = DFrequency (Frequency a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DForce a = DForce (Force a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>data instance DUnit DTorque a = DTorque (Torque a)
>   deriving (Show, Read, Eq)
>data instance DUnit DEnergy a = DEnergy (Energy a)
>   deriving (Show, Read,Eq)

>instance (Num a) => VectorSpace (DUnit DTorque a) where
>   type Scalar (DUnit DTorque a) = a
>   vzero = DTorque vzero
>   vnegate (DTorque x) = DTorque (vnegate x)
>   (DTorque x) %+ (DTorque y) = DTorque (x %+ y)
>   k %* (DTorque x) = DTorque (k %* x)

>instance (Num a) => VectorSpace (DUnit DEnergy a) where
>   type Scalar (DUnit DEnergy a) = a
>   vzero = DEnergy vzero
>   vnegate (DEnergy x) = DEnergy (vnegate x)
>   (DEnergy x) %+ (DEnergy y) = DEnergy (x %+ y)
>   k %* (DEnergy x) = DEnergy (k %* x)

>newtype instance DUnit DPressure a = DPressure (Pressure a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)

>newtype instance DUnit DPower a = DPower (Power a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DCharge a = DCharge (Charge a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DVoltage a = DVoltage (Voltage a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DCapacitance a = DCapacitance (Capacitance a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DResistance a = DResistance (Resistance a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>data instance DUnit DConductance a = DConductance (Conductance a)
>                                 | DEquivalentDose (EquivalentDose a)
>                                 | DZeroConductance
>   deriving (Show, Read,Eq)
>instance (Num a) => VectorSpace (DUnit DConductance a) where
>   type Scalar (DUnit DConductance a) = a
>   vzero = DZeroConductance
>   vnegate (DConductance a) = DConductance (vnegate a)
>   vnegate DZeroConductance = DZeroConductance
>   (DConductance a) %+ (DConductance b) = DConductance (a %+ b)
>   (DEquivalentDose a) %+ (DEquivalentDose b) = DEquivalentDose (a %+ b)
>   DZeroConductance %+ a = a
>   a %+ DZeroConductance = a
>   _ %+ _ = error "ill-typed DUnit DConductance addition"
>   k %* (DConductance a) = DConductance (k %* a)
>   k %* (DEquivalentDose a) = DEquivalentDose (k %* a)
>   k %* DZeroConductance = DZeroConductance
>newtype instance DUnit DFlux a = DFlux (Flux a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace, LiteralUnit)
>newtype instance DUnit DFluxDensity a = DFluxDensity (FluxDensity a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DInductance a = DInductance (Inductance a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DIlluminance a = DIlluminance (Illuminance a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DRadioactivity a = DRadioactivity (Radioactivity a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DAbsorbedDose a = DAbsorbedDose (AbsorbedDose a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DCatalyticActivity a = DCatalyticActivity (CatalyticActivity a)
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)

>instance (Num a, Show a) => Unit (DUnit DLength a) where
>  amount (DLength x) = amount x
>  fromQuantity q = fromQuantity q >>= (return . DLength)
>  unitOf (DLength x) = unitOf x
>  dimension (DLength x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DLength a) where
>  fromAmount s = DLength (fromAmount s)

>instance (Num a, Show a) => Unit (DUnit DTime a) where
>  amount (DTime x) = amount x
>  fromQuantity q = fromQuantity q >>= (return . DTime)
>  unitOf (DTime x) = unitOf x
>  dimension (DTime x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DTime a) where
>  fromAmount s = DTime (fromAmount s)
> 
>instance (Num a, Show a) => Unit (DUnit DSquareLength a) where
>   amount (DSquareLength a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DSquareLength)
>   unitOf (DSquareLength x) = unitOf x
>   dimension (DSquareLength x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DSquareLength a) where
>   fromAmount s = DSquareLength (fromAmount s)

>instance (Num a, Show a) => Unit (DUnit DCubicLength a) where
>   amount (DCubicLength a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DCubicLength)
>   unitOf (DCubicLength x) = unitOf x
>   dimension (DCubicLength x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DCubicLength a) where
>   fromAmount s = DCubicLength (fromAmount s)

>instance (Num a, Show a) => Unit (DUnit DMass a) where
>   amount (DMass a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DMass)
>   unitOf (DMass x) = unitOf x
>   dimension (DMass x) = dimension x
>instance (Num a, Show a) =>  LiteralUnit (DUnit DMass a) where
>   fromAmount s = DMass (fromAmount s)
> 
>instance (Num a, Show a) => Unit (DUnit DCurrent a) where
>   amount (DCurrent a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DCurrent)
>   unitOf (DCurrent x) = unitOf x
>   dimension (DCurrent x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DCurrent a) where
>   fromAmount s = DCurrent (fromAmount s)

>instance (Num a, Show a) => Unit (DUnit DVelocity a) where
>   amount (DVelocity x) = amount x
>   fromQuantity q = fromQuantity q >>= (return . DVelocity)
>   unitOf (DVelocity x) = unitOf x
>   dimension (DVelocity x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DVelocity a) where
>   fromAmount s = DVelocity (fromAmount s)
>
>instance (Num a, Show a) => Unit (DUnit DAcceleration a) where
>   amount (DAcceleration x) = amount x
>   fromQuantity q = fromQuantity q >>= (return . DAcceleration)
>   unitOf (DAcceleration x) = unitOf x
>   dimension (DAcceleration x) = dimension x

instance LiteralUnit (DUnit DAcceleration) where
   fromAmount s = DAcceleration (fromAmount s)

>instance (Num a, Show a) => Unit (DUnit DSubstance a) where
>   amount (DSubstance a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DSubstance)
>   unitOf (DSubstance x) = unitOf x
>   dimension (DSubstance x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DSubstance a) where
>   fromAmount s = DSubstance (fromAmount s)
> 
>instance (Num a, Show a) => Unit (DUnit DFrequency a) where
>   amount (DFrequency a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DFrequency)
>   unitOf (DFrequency x) = unitOf x
>   dimension (DFrequency x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DFrequency a) where
>   fromAmount s = DFrequency (fromAmount s)

>instance (Num a, Show a) =>  Unit (DUnit DForce a) where
>   amount (DForce a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DForce)
>   unitOf (DForce x) = unitOf x
>   dimension (DForce x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DForce a) where
>   fromAmount s = DForce (fromAmount s)
>instance (Num a, Show a) => Unit (DUnit DPressure a) where
>   amount (DPressure a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DPressure)
>   unitOf (DPressure x) = unitOf x
>   dimension (DPressure x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DPressure a) where
>   fromAmount s = DPressure (fromAmount s)
>instance (Num a, Show a) => Unit (DUnit DPower a) where
>   amount (DPower a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DPower)
>   unitOf (DPower x) = unitOf x
>   dimension (DPower x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DPower a) where
>   fromAmount s = DPower (fromAmount s)
>instance (Num a, Show a) => Unit (DUnit DCharge a) where
>   amount (DCharge a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DCharge)
>   unitOf (DCharge x) = unitOf x
>   dimension (DCharge x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DCharge a) where
>   fromAmount s = DCharge (fromAmount s)
>instance (Num a, Show a) => Unit (DUnit DVoltage a) where
>   amount (DVoltage a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DVoltage)
>   unitOf (DVoltage x) = unitOf x
>   dimension (DVoltage x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DVoltage a) where
>   fromAmount s = DVoltage (fromAmount s)
>instance (Num a, Show a) => Unit (DUnit DCapacitance a) where
>   amount (DCapacitance a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DCapacitance)
>   unitOf (DCapacitance x) = unitOf x
>   dimension (DCapacitance x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DCapacitance a) where
>   fromAmount s = DCapacitance (fromAmount s)
>instance (Num a, Show a) => Unit (DUnit DResistance a) where
>   amount (DResistance a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DResistance)
>   unitOf (DResistance x) = unitOf x
>   dimension (DResistance x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DResistance a) where
>   fromAmount s = DResistance (fromAmount s)
>instance (Num a, Show a) => Unit (DUnit DFlux a) where
>   amount (DFlux a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DFlux)
>   unitOf (DFlux x) = unitOf x
>   dimension (DFlux x) = dimension x

instance LiteralUnit (DUnit DFlux) where
   fromAmount s = DFlux (fromAmount s)

>instance (Num a, Show a) => Unit (DUnit DFluxDensity a) where
>   amount (DFluxDensity a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DFluxDensity)
>   unitOf (DFluxDensity x) = unitOf x
>   dimension (DFluxDensity x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DFluxDensity a) where
>   fromAmount s = DFluxDensity (fromAmount s)
>instance (Num a, Show a) => Unit (DUnit DInductance a) where
>   amount (DInductance a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DInductance)
>   unitOf (DInductance x) = unitOf x
>   dimension (DInductance x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DInductance a) where
>   fromAmount s = DInductance (fromAmount s)
>instance (Num a, Show a) => Unit (DUnit DIlluminance a) where
>   amount (DIlluminance a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DIlluminance)
>   unitOf (DIlluminance x) = unitOf x
>   dimension (DIlluminance x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DIlluminance a) where
>   fromAmount s = DIlluminance (fromAmount s)
>instance (Num a, Show a) => Unit (DUnit DRadioactivity a) where
>   amount (DRadioactivity a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DRadioactivity)
>   unitOf (DRadioactivity x) = unitOf x
>   dimension (DRadioactivity x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DRadioactivity a) where
>   fromAmount s = DRadioactivity (fromAmount s)

>instance (Num a, Show a) => Unit (DUnit DAbsorbedDose a) where
>   amount (DAbsorbedDose a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DAbsorbedDose)
>   unitOf (DAbsorbedDose x) = unitOf x
>   dimension (DAbsorbedDose x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DAbsorbedDose a) where
>   fromAmount s = DAbsorbedDose (fromAmount s)

>instance (Num a, Show a) => Unit (DUnit DCatalyticActivity a) where
>   amount (DCatalyticActivity a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DCatalyticActivity)
>   unitOf (DCatalyticActivity x) = unitOf x
>   dimension (DCatalyticActivity x) = dimension x
>instance (Num a, Show a) => LiteralUnit (DUnit DCatalyticActivity a) where
>   fromAmount s = DCatalyticActivity (fromAmount s)
