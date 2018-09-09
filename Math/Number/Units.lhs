>{-# LANGUAGE CPP,GeneralizedNewtypeDeriving, TypeFamilies, DataKinds, TypeOperators, TypeInType #-}
>{-# LANGUAGE ExistentialQuantification, GADTs, PolyKinds, UnicodeSyntax #-}
>{-# LANGUAGE FlexibleContexts #-}
>-- | This module contains auxiliary definitions related to dimensional analysis.
>--   This is based on the SI system of units.
>--   <https://en.wikipedia.org/wiki/International_System_of_Units>
>module Math.Number.Units where
>import Data.Ratio
>import Math.Matrix.Interface
>import Math.Number.DimensionalAnalysis

>newtype Angle = Radians { radians :: Double }
>   deriving (Read,Eq,Ord)
>instance Show Angle where { show = show_unit }

>instance VectorSpace Angle where
>   type Scalar Angle = Double
>   vzero = Radians 0
>   vnegate (Radians x) = Radians (negate x)
>   (Radians x) %+ (Radians y) = Radians $ x + y
>   k %* (Radians x) = Radians $ k * x

>degreesAngle :: Double -> Angle
>degreesAngle d = Radians $ (d * pi) / 180.0

>degreesCelcius :: Angle -> Double
>degreesCelcius (Radians r) = r * 180.0 / pi

>instance Unit Angle where
>   amount = radians
>   fromAmount = Radians
>   dimension _ = dimensionless
>   unitOf _ = "rad"

>-- | <https://en.wikipedia.org/wiki/Steradian>
>newtype SolidAngle = Steradians { steradians :: Double }
>   deriving (Read,Eq,Ord)

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
>   unitOf _ = "sr"

>show_unit :: (Unit u, Show (Scalar u)) => u -> String
>show_unit i = show (amount i) ++ " " ++ unitOf i

>-- types for basic units <https://en.wikipedia.org/wiki/International_System_of_Units>
>newtype Length = Meter { meters :: Double } deriving (Eq,Ord)
>instance Show Length where { show = show_unit }
>instance VectorSpace Length where
>   type Scalar Length = Double
>   vzero = Meter 0
>   vnegate (Meter x) = Meter $ negate x
>   (Meter x) %+ (Meter y) = Meter $ x + y
>   k %* (Meter x) = Meter $ k * x

>newtype Mass = Kilogram { kilograms :: Double } deriving (Eq,Ord)
>instance Show Mass where { show = show_unit }
>instance VectorSpace Mass where
>   type Scalar Mass = Double
>   vzero = Kilogram 0
>   vnegate (Kilogram x) = Kilogram $ negate x
>   (Kilogram x) %+ (Kilogram y) = Kilogram $ x + y
>   k %* (Kilogram x) = Kilogram $ k * x
>newtype Time = Second { seconds :: Double } deriving (Eq,Ord)
>instance Show Time where { show = show_unit }

>instance VectorSpace Time where
>   type Scalar Time = Double
>   vzero = Second 0
>   vnegate (Second x) = Second $ negate x
>   (Second x) %+ (Second y) = Second $ x + y
>   k %* (Second x) = Second $ k * x
>newtype Current = Ampere { amperes :: Double } deriving (Eq,Ord)
>instance Show Current where { show = show_unit }

>instance VectorSpace Current where
>   type Scalar Current = Double
>   vzero = Ampere 0
>   vnegate (Ampere x) = Ampere $ negate x
>   (Ampere x) %+ (Ampere y) = Ampere $ x + y
>   k %* (Ampere x) = Ampere $ k * x
>newtype Temperature = Kelvin { kelvins :: Double } deriving (Eq,Ord)
>instance Show Temperature where { show = show_unit }

>instance VectorSpace Temperature where
>   type Scalar Temperature = Double
>   vzero = Kelvin 0
>   vnegate (Kelvin x) = Kelvin $ negate x
>   (Kelvin x) %+ (Kelvin y) = Kelvin $ x + y
>   k %* (Kelvin x) = Kelvin $ k * x
>newtype Substance = Mole { moles :: Double } deriving (Eq,Ord)
>instance Show Substance where { show = show_unit }

>instance VectorSpace Substance where
>   type Scalar Substance = Double
>   vzero = Mole 0
>   vnegate (Mole x) = Mole $ negate x
>   (Mole x) %+ (Mole y) = Mole $ x + y
>   k %* (Mole x) = Mole $ k * x
>newtype Intensity = Candela { candelas :: Double } deriving (Eq,Ord)
>instance Show Intensity where { show = show_unit }

>instance VectorSpace Intensity where
>   type Scalar Intensity = Double
>   vzero = Candela 0
>   vnegate (Candela x) = Candela $ negate x
>   (Candela x) %+ (Candela y) = Candela $ x + y
>   k %* (Candela x) = Candela $ k * x
>newtype Frequency = Hertz { hertzs :: Double } deriving (Eq,Ord)
>instance Show Frequency where { show = show_unit }

>instance VectorSpace Frequency where
>   type Scalar Frequency = Double
>   vzero = Hertz 0
>   vnegate (Hertz x) = Hertz $ negate x
>   (Hertz x) %+ (Hertz y) = Hertz $ x + y
>   k %* (Hertz x) = Hertz $ k * x
>newtype Force = Newton { newtons :: Double } deriving (Eq,Ord)
>instance Show Force where { show = show_unit }

>instance VectorSpace Force where
>   type Scalar Force = Double
>   vzero = Newton 0
>   vnegate (Newton x) = Newton $ negate x
>   (Newton x) %+ (Newton y) = Newton $ x + y
>   k %* (Newton x) = Newton $ k * x
>newtype Pressure = Pascal { pascals :: Double } deriving (Eq,Ord)
>instance Show Pressure where { show = show_unit }

>instance VectorSpace Pressure where
>   type Scalar Pressure = Double
>   vzero = Pascal 0
>   vnegate (Pascal x) = Pascal $ negate x
>   (Pascal x) %+ (Pascal y) = Pascal $ x + y
>   k %* (Pascal x) = Pascal $ k * x
>newtype Energy = Joule { joules :: Double } deriving (Eq,Ord)
>instance Show Energy where { show = show_unit }

>instance VectorSpace Energy where
>   type Scalar Energy = Double
>   vzero = Joule 0
>   vnegate (Joule x) = Joule $ negate x
>   (Joule x) %+ (Joule y) = Joule $ x + y
>   k %* (Joule x) = Joule $ k * x
>newtype Power = Watt { watts :: Double } deriving (Eq,Ord)
>instance Show Power where { show = show_unit }

>instance VectorSpace Power where
>   type Scalar Power = Double
>   vzero = Watt 0
>   vnegate (Watt x) = Watt $ negate x
>   (Watt x) %+ (Watt y) = Watt $ x + y
>   k %* (Watt x) = Watt $ k * x
>newtype Charge = Coulomb { coulombs :: Double } deriving (Eq,Ord)
>instance Show Charge where { show = show_unit }

>instance VectorSpace Charge where
>   type Scalar Charge = Double
>   vzero = Coulomb 0
>   vnegate (Coulomb x) = Coulomb $ negate x
>   (Coulomb x) %+ (Coulomb y) = Coulomb $ x + y
>   k %* (Coulomb x) = Coulomb $ k * x
>newtype Voltage = Volt { volts :: Double } deriving (Eq,Ord)
>instance Show Voltage where { show = show_unit }

>instance VectorSpace Voltage where
>   type Scalar Voltage = Double
>   vzero = Volt 0
>   vnegate (Volt x) = Volt $ negate x
>   (Volt x) %+ (Volt y) = Volt $ x + y
>   k %* (Volt x) = Volt $ k * x
>newtype Capacitance = Farad { farads :: Double } deriving (Eq,Ord)
>instance Show Capacitance where { show = show_unit }

>instance VectorSpace Capacitance where
>   type Scalar Capacitance = Double
>   vzero = Farad 0
>   vnegate (Farad x) = Farad $ negate x
>   (Farad x) %+ (Farad y) = Farad $ x + y
>   k %* (Farad x) = Farad $ k * x
>newtype Resistance = Ohm { ohms :: Double } deriving (Eq,Ord)
>instance Show Resistance where { show = show_unit }

>instance VectorSpace Resistance where
>   type Scalar Resistance = Double
>   vzero = Ohm 0
>   vnegate (Ohm x) = Ohm $ negate x
>   (Ohm x) %+ (Ohm y) = Ohm $ x + y
>   k %* (Ohm x) = Ohm $ k * x
>newtype Conductance = Siemens { siemenses :: Double } deriving (Eq,Ord)
>instance Show Conductance where { show = show_unit }

>instance VectorSpace Conductance where
>   type Scalar Conductance = Double
>   vzero = Siemens 0
>   vnegate (Siemens x) = Siemens $ negate x
>   (Siemens x) %+ (Siemens y) = Siemens $ x + y
>   k %* (Siemens x) = Siemens $ k * x
>newtype Flux = Weber { webers :: Double } deriving (Eq,Ord)
>instance Show Flux where { show = show_unit }

>instance VectorSpace Flux where
>   type Scalar Flux = Double
>   vzero = Weber 0
>   vnegate (Weber x) = Weber $ negate x
>   (Weber x) %+ (Weber y) = Weber $ x + y
>   k %* (Weber x) = Weber $ k * x
>newtype FluxDensity = Tesla { teslas :: Double } deriving (Eq,Ord)
>instance Show FluxDensity where { show = show_unit }

>instance VectorSpace FluxDensity where
>   type Scalar FluxDensity = Double
>   vzero = Tesla 0
>   vnegate (Tesla x) = Tesla $ negate x
>   (Tesla x) %+ (Tesla y) = Tesla $ x + y
>   k %* (Tesla x) = Tesla $ k * x
>newtype Inductance  = Henry { henrys :: Double } deriving (Eq,Ord)
>instance Show Inductance where { show = show_unit }

>instance VectorSpace Inductance where
>   type Scalar Inductance = Double
>   vzero = Henry 0
>   vnegate (Henry x) = Henry $ negate x
>   (Henry x) %+ (Henry y) = Henry $ x + y
>   k %* (Henry x) = Henry $ k * x
>newtype LuminousFlux = Lumen { lumens :: Double } deriving (Eq,Ord)
>instance Show LuminousFlux where { show = show_unit }

>instance VectorSpace LuminousFlux where
>   type Scalar LuminousFlux = Double
>   vzero = Lumen 0
>   vnegate (Lumen x) = Lumen $ negate x
>   (Lumen x) %+ (Lumen y) = Lumen $ x + y
>   k %* (Lumen x) = Lumen $ k * x
>newtype Illuminance = Lux { luxes :: Double } deriving (Eq,Ord)
>instance Show Illuminance where { show = show_unit }

>instance VectorSpace Illuminance where
>   type Scalar Illuminance = Double
>   vzero = Lux 0
>   vnegate (Lux x) = Lux $ negate x
>   (Lux x) %+ (Lux y) = Lux $ x + y
>   k %* (Lux x) = Lux $ k * x
>newtype Radioactivity = Becquerel { becquerels :: Double } deriving (Eq,Ord)
>instance Show Radioactivity where { show = show_unit }

>instance VectorSpace Radioactivity where
>   type Scalar Radioactivity = Double
>   vzero = Becquerel 0
>   vnegate (Becquerel x) = Becquerel $ negate x
>   (Becquerel x) %+ (Becquerel y) = Becquerel $ x + y
>   k %* (Becquerel x) = Becquerel $ k * x
>newtype AbsorbedDose  = Gray { grays :: Double } deriving (Eq,Ord)
>instance Show AbsorbedDose where { show = show_unit }

>instance VectorSpace AbsorbedDose where
>   type Scalar AbsorbedDose = Double
>   vzero = Gray 0
>   vnegate (Gray x) = Gray $ negate x
>   (Gray x) %+ (Gray y) = Gray $ x + y
>   k %* (Gray x) = Gray $ k * x
>newtype EquivalentDose = Sievert { sieverts :: Double } deriving (Eq,Ord)
>instance Show EquivalentDose where { show = show_unit }

>instance VectorSpace EquivalentDose where
>   type Scalar EquivalentDose = Double
>   vzero = Sievert 0
>   vnegate (Sievert x) = Sievert $ negate x
>   (Sievert x) %+ (Sievert y) = Sievert $ x + y
>   k %* (Sievert x) = Sievert $ k * x

>newtype CatalyticActivity = Katal { katals :: Double } deriving (Eq,Ord)
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
>   unitOf _ = "kat"
>instance Unit EquivalentDose where
>   amount = sieverts
>   fromAmount = Sievert
>   dimension _ = sievert_dimension
>   unitOf _ = "Sv"
>instance Unit AbsorbedDose where
>   amount = grays
>   fromAmount = Gray
>   dimension _ = gray_dimension
>   unitOf _ = "Gy"
>instance Unit Radioactivity where
>   amount = becquerels
>   fromAmount = Becquerel
>   dimension _ = becquerel_dimension
>   unitOf _ = "Bq"
>instance Unit Illuminance where
>   amount = luxes
>   fromAmount = Lux
>   dimension _ = lux_dimension
>   unitOf _ = "lx"
>instance Unit LuminousFlux where
>   amount = lumens
>   fromAmount = Lumen
>   dimension _ = lumen_dimension
>   unitOf _ = "lm"
>instance Unit Inductance where
>   amount = henrys
>   fromAmount = Henry
>   dimension _ = henry_dimension
>   unitOf _ = "H"
>instance Unit FluxDensity where
>   amount = teslas
>   fromAmount = Tesla
>   dimension _ = tesla_dimension
>   unitOf _ = "T"
>instance Unit Flux where
>   amount = webers
>   fromAmount = Weber
>   dimension _ = weber_dimension
>   unitOf _ = "W"
>instance Unit Conductance where
>   amount = siemenses
>   fromAmount = Siemens
>   dimension _ = siemens_dimension
>   unitOf _ = "S"
>instance Unit Resistance where
>   amount = ohms
>   fromAmount = Ohm
>   dimension _ = ohm_dimension
>   unitOf _ = "Ω"
>instance Unit Capacitance where
>   amount = farads
>   fromAmount = Farad
>   dimension _ = farad_dimension
>   unitOf _ = "F"
>instance Unit Voltage where
>   amount = volts
>   fromAmount = Volt
>   dimension _ = volt_dimension
>   unitOf _ = "V"
>instance Unit Charge where
>   amount = coulombs
>   fromAmount = Coulomb
>   dimension _ = coulomb_dimension
>   unitOf _ = "C"
>instance Unit Power where
>   amount = watts
>   fromAmount = Watt
>   dimension _ = watt_dimension
>   unitOf _ = "W"
>instance Unit Energy where
>   amount = joules
>   fromAmount = Joule
>   dimension _ = joule_dimension
>   unitOf _ = "J"
>instance Unit Pressure where
>   amount = pascals
>   fromAmount = Pascal
>   dimension _ = pascal_dimension
>   unitOf _ = "Pa"
>instance Unit Force where
>   amount = newtons
>   fromAmount = Newton
>   dimension _ = newton_dimension
>   unitOf _ = "N"
>instance Unit Frequency where
>   amount = hertzs
>   fromAmount = Hertz
>   dimension _ = hertz_dimension
>   unitOf _ = "Hz"
>instance Unit Length where
>   amount = meters
>   fromAmount = Meter
>   dimension _ = meter_dimension
>   unitOf _ = "m"
>instance Unit Mass where
>   amount = kilograms
>   fromAmount = Kilogram
>   dimension _ = kilogram_dimension
>   unitOf _ = "kg"
>instance Unit Time where
>   amount = seconds
>   fromAmount = Second
>   dimension _ = second_dimension
>   unitOf _ = "s"
>instance Unit Current where
>   amount = amperes
>   fromAmount = Ampere
>   dimension _ = ampere_dimension
>   unitOf _ = "A"
>instance Unit Temperature where
>   amount = kelvins
>   fromAmount = Kelvin
>   dimension _ = kelvin_dimension
>   unitOf _ = "K"
>instance Unit Substance where
>   amount = moles
>   fromAmount = Mole
>   dimension _ = mol_dimension
>   unitOf _ = "mol"
>instance Unit Intensity where
>   amount = candelas
>   fromAmount = Candela
>   dimension _ = candela_dimension
>   unitOf _ = "cd"

>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>point = (127/360) %* milli meter
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>pica = 12 %* point
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>inch = 6 %* pica
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>foot = 12 %* inch
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>feet = foot
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>yard = 3 %* foot
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>mile = 1760 %* yard
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>link = (33/50) %* foot
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>surveyFoot = (1200/3937) %* meter
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>rod = 25 %* link
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>chain = 4 %* rod
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>furlong = 10 %* chain
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>surveyMile = 8 %* furlong
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>league = 3 %* mile
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>fathom = 2 %* yard
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>

>cable = 120 %* fathom
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>nauticalMile = 1.852 %* kilo meter

>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>squareSurveyFoot = 144 %* (inch * inch)
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>squareChain = 4356 %* squareSurveyFoot
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>acre = 43560 %* squareSurveyFoot
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>section = 640 %* acre
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>surveyTownship = 36 %* section
>
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>cubicInch = 16.387064 %* milli liter
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>cubicFoot = 28.316846592 %* liter
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>cubicYard = 764.554857984 %* liter
> 
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>grain = 64.79891 %* milli gram
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units> 
>dram = 1.7718451953125 %* gram
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>ounce = 16 %* dram
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units> 
>pound = 16 %* ounce
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>usHundredWeight = 100 %* pound
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>longHundredWeight = 112 %* pound
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>ton = 2000 %* pound
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>longTon = 2240 %* pound
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>pennyWeight = 24 %* grain
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>troyOunce = 20 %* pennyWeight
>-- | <https://en.wikipedia.org/wiki/United_States_customary_units>
>troyPound = 12 %* troyOunce
