>{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 800

>{-# LANGUAGE DataKinds, TypeOperators, MultiParamTypeClasses, KindSignatures #-}
>{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, GADTs, UndecidableInstances #-}
>{-# LANGUAGE FlexibleContexts #-}
>module Math.Number.TypeUnits where
>import Data.Kind
>import Math.Matrix.Interface
>import Math.Number.DimensionalAnalysis
>import Math.Number.Units
>import Math.Number.TypeRational

>data SDimension = SDimension {
>   smeter :: SNat,
>   skilogram :: SNat,
>   ssecond :: SNat,
>   sampere :: SNat,
>   skelvin :: SNat,
>   scandela :: SNat,
>   smole :: SNat }

>(*!) :: (Scalar (U d) ~ Double, (Scalar (U d') ~ Double),
>         Unit (U d), Unit (U d'), Unit (U (DPlus d d')),
>         Scalar (U (DPlus d d')) ~ Double)
>        => U d -> U d' -> U (DPlus d d')
>(*!) x y = fromAmount (amount x * amount y)

>(/!) :: (Scalar (U d) ~ Double, Scalar (U d') ~ Double
>         ,Unit (U d), Unit (U d'),
>         Unit (U (DMinus d d')),
>         (Double ~ Scalar (U (DMinus d d'))))
>        => U d -> U d' -> U (DMinus d d')
>(/!) x y = fromAmount (amount x / amount y)


>type family U (dims :: SDimension) = res | res -> dims
>type instance U SMeterDim = Length
>type instance U SKilogramDim = Mass
>type instance U SSecondDim = Time
>type instance U SAmpereDim = Current
>type instance U SKelvinDim = Temperature
>type instance U SCandelaDim = Intensity
>type instance U SMoleDim = Substance
>type instance U SFaradDim = Capacitance
>type instance U SOhmDim = Resistance
>type instance U SSievertDim = EquivalentDose
>type instance U SHenryDim   = Inductance
>type instance U SWeberDim  = Flux
>type instance U STeslaDim   = FluxDensity

>type family DPlus (d :: SDimension) (d' :: SDimension) where
>   DPlus ('SDimension m k s a kk ca mo) ('SDimension m' k' s' a' kk' ca' mo') = 'SDimension (SPlus m m') (SPlus k k') (SPlus s s') (SPlus a a') (SPlus kk kk') (SPlus ca ca') (SPlus mo mo')

>type family DMinus (d :: SDimension) (d' :: SDimension) where
>   DMinus ('SDimension m k s a kk ca mo) ('SDimension m' k' s' a' kk' ca' mo') = 'SDimension (SMinus m m') (SMinus k k') (SMinus s s') (SMinus a a') (SMinus kk kk') (SMinus ca ca') (SMinus mo mo')

>type family DScalarProd (k :: SNat) (d :: SDimension) where
>   DScalarProd Ze d = 'SDimension 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze
>   DScalarProd (Su Ze) d = d
>   DScalarProd (Su x) d = DPlus d (DScalarProd x d)

> 
>type SDimensionless = 'SDimension 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze
>type SMeterDim = 'SDimension SOne 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze
>type SSquareMeterDim = 'SDimension STwo 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze
>type SKilogramDim = 'SDimension 'Ze SOne 'Ze 'Ze 'Ze 'Ze 'Ze
>type SSecondDim = 'SDimension 'Ze 'Ze SOne 'Ze 'Ze 'Ze 'Ze
>type SAmpereDim = 'SDimension 'Ze 'Ze 'Ze SOne 'Ze 'Ze 'Ze
>type SKelvinDim = 'SDimension 'Ze 'Ze 'Ze 'Ze SOne 'Ze 'Ze
>type SCandelaDim = 'SDimension 'Ze 'Ze 'Ze 'Ze 'Ze SOne 'Ze
>type SMoleDim = 'SDimension 'Ze 'Ze 'Ze 'Ze 'Ze 'Ze SOne
>type SFaradDim = 'SDimension SMinusTwo SMinusOne SFour STwo 'Ze 'Ze 'Ze
>type SOhmDim   = 'SDimension STwo SOne SMinusThree SMinusTwo 'Ze 'Ze 'Ze
>type SSievertDim = 'SDimension SMinusTwo SMinusOne SThree STwo 'Ze 'Ze 'Ze
>type SVoltDim = 'SDimension STwo SOne SMinusThree SMinusOne 'Ze 'Ze 'Ze
>type SPascalDim = 'SDimension SMinusOne SOne SMinusTwo 'Ze 'Ze 'Ze 'Ze
>type SHenryDim = 'SDimension STwo SOne SMinusTwo SMinusTwo 'Ze 'Ze 'Ze
>type SWeberDim = 'SDimension STwo SOne SMinusTwo SMinusOne 'Ze 'Ze 'Ze
>type STeslaDim = 'SDimension 'Ze SOne SMinusTwo SMinusOne 'Ze 'Ze 'Ze
>type SBecquerelDim = DMinus SDimensionless SSecondDim
>type SGrayDim = DMinus (DPlus SMeterDim SMeterDim) (DPlus SSecondDim SSecondDim)
>type SKatalDim = DMinus SMoleDim SSecondDim
>type SLumenDim = SCandelaDim
>type SLuxDim = DMinus SCandelaDim (DPlus SMeterDim SMeterDim)

>data SDimensions :: SDimension -> * where
>   SMeter :: SDimensions SMeterDim
>   SKilogram :: SDimensions SKilogramDim
>   SSecond :: SDimensions SSecondDim
>   SAmpere :: SDimensions SAmpereDim
>   SKelvin :: SDimensions SKelvinDim
>   SCandela :: SDimensions SCandelaDim
>   SMole :: SDimensions SMoleDim
>   SFarad :: SDimensions SFaradDim
>   SOhm   :: SDimensions SOhmDim
>   SSievert :: SDimensions SSievertDim
>   SVolt :: SDimensions SVoltDim
>   SHenry :: SDimensions SHenryDim
>   SWeber :: SDimensions SWeberDim
>   STesla :: SDimensions STeslaDim
>   SLumen :: SDimensions SLumenDim
>   SLux   :: SDimensions SLuxDim
>   SBecquerel :: SDimensions SBecquerelDim
>   SGray :: SDimensions SGrayDim
>   SKatal :: SDimensions SKatalDim
>   SProduct :: SDimensions d -> SDimensions d' -> SDimensions (DPlus d d')

(*%) :: (Alternative m, Monad m, Show (Scalar u), Unit w, Unit u, Unit x,
         Scalar u ~ Scalar w, Scalar w ~ Scalar x) => u -> x -> m w

#endif
