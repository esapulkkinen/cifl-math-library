>{-# LANGUAGE Unsafe, OverloadedLists, TypeFamilies #-}
>{-# LANGUAGE OverloadedStrings, TypeOperators #-}
>{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
>{-# LANGUAGE UndecidableInstances #-}
>module Math.Number.Unsafe where
>-- ^ This module provides IsList and IsString instances for
>-- types where that is reasonable. To use them, you enable OverloadedStrings extension.
>-- and import this module. Notice that is not consided "safe haskell".
>-- 
>-- This allows writing for example:
>-- > ("3 kg" :: Mass) %+ ("5 kg" :: Mass)
>-- > 8.0 kg
>import GHC.Exts (IsList(..), IsString(..))
>import safe Math.Number.Stream
>import safe Math.Number.DimensionalAnalysis
>import safe Math.Number.Units
>import safe Math.Matrix.Interface
> 
>instance IsList (Stream a) where
>  type Item (Stream a) = a
>  fromList = Math.Number.Stream.cycle
>  toList = Math.Number.Stream.toList

>instance IsString (Quantity Double) where { fromString = read }
>instance IsString Angle where { fromString = read }
>instance IsString Resistance where { fromString = read }
>instance IsString DegreesAngle where { fromString = read }
>instance IsString Information where { fromString = read }
>instance IsString Inductance where { fromString = read }
>instance IsString EquivalentDose where { fromString = read }
>instance IsString Conductance where { fromString = read }
>instance IsString CatalyticActivity where { fromString = read }
>instance IsString Radioactivity where { fromString = read }
>instance IsString Illuminance where { fromString = read }
>instance IsString AbsorbedDose where { fromString = read }
>instance IsString FluxDensity where { fromString = read }
>instance IsString Flux where { fromString = read }
>instance IsString LuminousFlux where { fromString = read }
>instance IsString Capacitance where { fromString = read }
>instance IsString Charge where { fromString = read }
>instance IsString Power where { fromString = read }
>instance IsString Voltage where { fromString = read }
>instance IsString Energy where { fromString = read }
>instance IsString Pressure where { fromString = read }
>instance IsString Force where { fromString = read }
>instance IsString Intensity where { fromString = read }
>instance IsString Substance where { fromString = read }
>instance IsString Temperature where { fromString = read }
>instance IsString Frequency where { fromString = read }
>instance IsString SolidAngle where { fromString = read }
>instance IsString DegreesTemperature where { fromString = read }
>instance IsString Current where { fromString = read }
>instance IsString DegreesFahrenheit where { fromString = read }
>instance IsString Percentage where { fromString = read }
>instance IsString Time where { fromString = read }
>instance IsString Length where { fromString = read }
>instance IsString Mass where { fromString = read }
>instance IsString SquareLength where { fromString = read }
>instance IsString Acceleration where { fromString = read }
>instance IsString Velocity where { fromString = read }
>instance IsString Torque where { fromString = read }
>instance IsString SoundLevel where { fromString = read }
>instance (Unit a, Unit b, Show (Scalar a), Read (Scalar a), Scalar a ~ Scalar b) => IsString (a :* b) where { fromString = read }
>instance (Unit a, Unit b, Show (Scalar a), Read (Scalar a), Scalar a ~ Scalar b) => IsString (a :/ b) where { fromString = read }

