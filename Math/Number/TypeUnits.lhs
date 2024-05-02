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
>unit_multiply :: (Scalar (DUnit (DimensionPlus u u')) ~ Double,
>     Scalar (DUnit u) ~ Double,
>     Scalar (DUnit u') ~ Double,
>     Unit (DUnit u), Unit (DUnit u'), LiteralUnit (DUnit (DimensionPlus u u')))
>  => DUnit u -> DUnit u' -> DUnit (DimensionPlus u u')
>unit_multiply a b = fromAmount (amount a * amount b) 

>unit_divide :: (Scalar (DUnit (DimensionMinus u u')) ~ Double,
>     Scalar (DUnit u) ~ Double,
>     Scalar (DUnit u') ~ Double,
>     Unit (DUnit u), Unit (DUnit u'), LiteralUnit (DUnit (DimensionMinus u u')))
>  => DUnit u -> DUnit u' -> DUnit (DimensionMinus u u')
>unit_divide a b = fromAmount (amount a / amount b)

>half_circle_angle :: DUnit DAngle
>half_circle_angle = DAngle $ Radians pi

>full_circle_angle :: DUnit DAngle
>full_circle_angle = DAngle $ Radians (2 * pi)
>
>type family (:#:) a (dim :: [DDimension])
>type instance (:#:) Double '[] = DUnit DDimensionless
>type instance (:#:) Double (a ': '[]) = DUnit a
>type instance (:#:) d (a ': b ': cr) = d :#: (DimensionPlus a b ': cr)
>
>type family (:/:) a (dim :: [DDimension])
>type instance (:/:) a '[] = a
>type instance (:/:) (DUnit a) (b ': '[]) = DUnit (DimensionMinus a b)
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
 
>data family DUnit (u :: DDimension)
>data instance DUnit DAngle = DAngle Angle
>                           | DDegreesAngle DegreesAngle
>                           | DAngleZero
>   deriving (Show, Read,Eq)

>newtype instance DUnit DSolidAngle = DSolidAngle SolidAngle
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)

>-- | DDimensionlessZero is needed to avoid type error in vzero.
>data instance DUnit DDimensionless = DDimensionless Dimensionless
>                                   | DInformation Information
>                                   | DSoundLevel SoundLevel
>                                   | DPercentage Percentage
>                                   | DDimensionlessZero
>   deriving (Show, Read,Eq, Typeable, Data, Generic)
>instance VectorSpace (DUnit DAngle) where
>   type Scalar (DUnit DAngle) = Double
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

>instance VectorSpace (DUnit DDimensionless) where
>   type Scalar (DUnit DDimensionless) = Scalar Dimensionless
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
>newtype instance DUnit DLength = DLength Length
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DSquareLength = DSquareLength SquareLength
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DCubicLength = DCubicLength CubicLength
>   deriving (Show, Read, Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DMass = DMass Mass
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DTime = DTime Time
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DCurrent = DCurrent Current
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DVelocity = DVelocity Velocity
>   deriving (Show, Read, Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DAcceleration = DAcceleration Acceleration
>   deriving (Show, Read, Eq)
>   deriving newtype (VectorSpace, LiteralUnit)
>data instance DUnit DTemperature = DDegreesFahrenheit DegreesFahrenheit
>                                 | DDegreesCelcius DegreesTemperature
>                                 | DDegreesKelvin Temperature
>                                 | DZeroTemperature
>   deriving (Show, Read,Eq)
>instance VectorSpace (DUnit DTemperature) where
>   type Scalar (DUnit DTemperature) = Double
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
>newtype instance DUnit DSubstance = DSubstance Substance
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>data instance DUnit DLuminosity = DLuminosity Intensity
>                                | DLuminousFlux LuminousFlux
>                                | DZeroLuminosity
>   deriving (Show, Read,Eq)
>instance VectorSpace (DUnit DLuminosity) where
>   type Scalar (DUnit DLuminosity) = Double
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
>newtype instance DUnit DFrequency = DFrequency Frequency
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DForce = DForce Force
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>data instance DUnit DTorque = DTorque Torque
>   deriving (Show, Read, Eq)
>data instance DUnit DEnergy = DEnergy Energy
>   deriving (Show, Read,Eq)

>instance VectorSpace (DUnit DTorque) where
>   type Scalar (DUnit DTorque) = Double
>   vzero = DTorque vzero
>   vnegate (DTorque x) = DTorque (vnegate x)
>   (DTorque x) %+ (DTorque y) = DTorque (x %+ y)
>   k %* (DTorque x) = DTorque (k %* x)

>instance VectorSpace (DUnit DEnergy) where
>   type Scalar (DUnit DEnergy) = Double
>   vzero = DEnergy vzero
>   vnegate (DEnergy x) = DEnergy (vnegate x)
>   (DEnergy x) %+ (DEnergy y) = DEnergy (x %+ y)
>   k %* (DEnergy x) = DEnergy (k %* x)

>newtype instance DUnit DPressure = DPressure Pressure
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)

>newtype instance DUnit DPower = DPower Power
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DCharge = DCharge Charge
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DVoltage = DVoltage Voltage
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DCapacitance = DCapacitance Capacitance
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DResistance = DResistance Resistance
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>data instance DUnit DConductance = DConductance Conductance
>                                 | DEquivalentDose EquivalentDose
>                                 | DZeroConductance
>   deriving (Show, Read,Eq)
>instance VectorSpace (DUnit DConductance) where
>   type Scalar (DUnit DConductance) = Double
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
>newtype instance DUnit DFlux = DFlux Flux
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace, LiteralUnit)
>newtype instance DUnit DFluxDensity = DFluxDensity FluxDensity
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DInductance = DInductance Inductance
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DIlluminance = DIlluminance Illuminance
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DRadioactivity = DRadioactivity Radioactivity
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DAbsorbedDose = DAbsorbedDose AbsorbedDose
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)
>newtype instance DUnit DCatalyticActivity = DCatalyticActivity CatalyticActivity
>   deriving (Show, Read,Eq)
>   deriving newtype (VectorSpace)

>instance Unit (DUnit DLength) where
>  amount (DLength x) = amount x
>  fromQuantity q = fromQuantity q >>= (return . DLength)
>  unitOf (DLength x) = unitOf x
>  dimension (DLength x) = dimension x
>instance LiteralUnit (DUnit DLength) where
>  fromAmount s = DLength (fromAmount s)

>instance Unit (DUnit DTime) where
>  amount (DTime x) = amount x
>  fromQuantity q = fromQuantity q >>= (return . DTime)
>  unitOf (DTime x) = unitOf x
>  dimension (DTime x) = dimension x
>instance LiteralUnit (DUnit DTime) where
>  fromAmount s = DTime (fromAmount s)
> 
>instance Unit (DUnit DSquareLength) where
>   amount (DSquareLength a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DSquareLength)
>   unitOf (DSquareLength x) = unitOf x
>   dimension (DSquareLength x) = dimension x
>instance LiteralUnit (DUnit DSquareLength) where
>   fromAmount s = DSquareLength (fromAmount s)

>instance Unit (DUnit DCubicLength) where
>   amount (DCubicLength a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DCubicLength)
>   unitOf (DCubicLength x) = unitOf x
>   dimension (DCubicLength x) = dimension x
>instance LiteralUnit (DUnit DCubicLength) where
>   fromAmount s = DCubicLength (fromAmount s)

>instance Unit (DUnit DMass) where
>   amount (DMass a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DMass)
>   unitOf (DMass x) = unitOf x
>   dimension (DMass x) = dimension x
>instance LiteralUnit (DUnit DMass) where
>   fromAmount s = DMass (fromAmount s)
> 
>instance Unit (DUnit DCurrent) where
>   amount (DCurrent a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DCurrent)
>   unitOf (DCurrent x) = unitOf x
>   dimension (DCurrent x) = dimension x
>instance LiteralUnit (DUnit DCurrent) where
>   fromAmount s = DCurrent (fromAmount s)

>instance Unit (DUnit DVelocity) where
>   amount (DVelocity x) = amount x
>   fromQuantity q = fromQuantity q >>= (return . DVelocity)
>   unitOf (DVelocity x) = unitOf x
>   dimension (DVelocity x) = dimension x
>instance LiteralUnit (DUnit DVelocity) where
>   fromAmount s = DVelocity (fromAmount s)
>
>instance Unit (DUnit DAcceleration) where
>   amount (DAcceleration x) = amount x
>   fromQuantity q = fromQuantity q >>= (return . DAcceleration)
>   unitOf (DAcceleration x) = unitOf x
>   dimension (DAcceleration x) = dimension x

instance LiteralUnit (DUnit DAcceleration) where
   fromAmount s = DAcceleration (fromAmount s)

>instance Unit (DUnit DSubstance) where
>   amount (DSubstance a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DSubstance)
>   unitOf (DSubstance x) = unitOf x
>   dimension (DSubstance x) = dimension x
>instance LiteralUnit (DUnit DSubstance) where
>   fromAmount s = DSubstance (fromAmount s)
> 
>instance Unit (DUnit DFrequency) where
>   amount (DFrequency a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DFrequency)
>   unitOf (DFrequency x) = unitOf x
>   dimension (DFrequency x) = dimension x
>instance LiteralUnit (DUnit DFrequency) where
>   fromAmount s = DFrequency (fromAmount s)

>instance Unit (DUnit DForce) where
>   amount (DForce a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DForce)
>   unitOf (DForce x) = unitOf x
>   dimension (DForce x) = dimension x
>instance LiteralUnit (DUnit DForce) where
>   fromAmount s = DForce (fromAmount s)
>instance Unit (DUnit DPressure) where
>   amount (DPressure a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DPressure)
>   unitOf (DPressure x) = unitOf x
>   dimension (DPressure x) = dimension x
>instance LiteralUnit (DUnit DPressure) where
>   fromAmount s = DPressure (fromAmount s)
>instance Unit (DUnit DPower) where
>   amount (DPower a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DPower)
>   unitOf (DPower x) = unitOf x
>   dimension (DPower x) = dimension x
>instance LiteralUnit (DUnit DPower) where
>   fromAmount s = DPower (fromAmount s)
>instance Unit (DUnit DCharge) where
>   amount (DCharge a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DCharge)
>   unitOf (DCharge x) = unitOf x
>   dimension (DCharge x) = dimension x
>instance LiteralUnit (DUnit DCharge) where
>   fromAmount s = DCharge (fromAmount s)
>instance Unit (DUnit DVoltage) where
>   amount (DVoltage a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DVoltage)
>   unitOf (DVoltage x) = unitOf x
>   dimension (DVoltage x) = dimension x
>instance LiteralUnit (DUnit DVoltage) where
>   fromAmount s = DVoltage (fromAmount s)
>instance Unit (DUnit DCapacitance) where
>   amount (DCapacitance a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DCapacitance)
>   unitOf (DCapacitance x) = unitOf x
>   dimension (DCapacitance x) = dimension x
>instance LiteralUnit (DUnit DCapacitance) where
>   fromAmount s = DCapacitance (fromAmount s)
>instance Unit (DUnit DResistance) where
>   amount (DResistance a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DResistance)
>   unitOf (DResistance x) = unitOf x
>   dimension (DResistance x) = dimension x
>instance LiteralUnit (DUnit DResistance) where
>   fromAmount s = DResistance (fromAmount s)
>instance Unit (DUnit DFlux) where
>   amount (DFlux a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DFlux)
>   unitOf (DFlux x) = unitOf x
>   dimension (DFlux x) = dimension x

instance LiteralUnit (DUnit DFlux) where
   fromAmount s = DFlux (fromAmount s)

>instance Unit (DUnit DFluxDensity) where
>   amount (DFluxDensity a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DFluxDensity)
>   unitOf (DFluxDensity x) = unitOf x
>   dimension (DFluxDensity x) = dimension x
>instance LiteralUnit (DUnit DFluxDensity) where
>   fromAmount s = DFluxDensity (fromAmount s)
>instance Unit (DUnit DInductance) where
>   amount (DInductance a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DInductance)
>   unitOf (DInductance x) = unitOf x
>   dimension (DInductance x) = dimension x
>instance LiteralUnit (DUnit DInductance) where
>   fromAmount s = DInductance (fromAmount s)
>instance Unit (DUnit DIlluminance) where
>   amount (DIlluminance a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DIlluminance)
>   unitOf (DIlluminance x) = unitOf x
>   dimension (DIlluminance x) = dimension x
>instance LiteralUnit (DUnit DIlluminance) where
>   fromAmount s = DIlluminance (fromAmount s)
>instance Unit (DUnit DRadioactivity) where
>   amount (DRadioactivity a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DRadioactivity)
>   unitOf (DRadioactivity x) = unitOf x
>   dimension (DRadioactivity x) = dimension x
>instance LiteralUnit (DUnit DRadioactivity) where
>   fromAmount s = DRadioactivity (fromAmount s)

>instance Unit (DUnit DAbsorbedDose) where
>   amount (DAbsorbedDose a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DAbsorbedDose)
>   unitOf (DAbsorbedDose x) = unitOf x
>   dimension (DAbsorbedDose x) = dimension x
>instance LiteralUnit (DUnit DAbsorbedDose) where
>   fromAmount s = DAbsorbedDose (fromAmount s)

>instance Unit (DUnit DCatalyticActivity) where
>   amount (DCatalyticActivity a) = amount a
>   fromQuantity q = fromQuantity q >>= (return . DCatalyticActivity)
>   unitOf (DCatalyticActivity x) = unitOf x
>   dimension (DCatalyticActivity x) = dimension x
>instance LiteralUnit (DUnit DCatalyticActivity) where
>   fromAmount s = DCatalyticActivity (fromAmount s)
