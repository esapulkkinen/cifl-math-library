>{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, DataKinds, TypeOperators, TypeInType #-}
>{-# LANGUAGE ExistentialQuantification, GADTs, PolyKinds #-}
>module Math.Number.Units where
>import Data.Ratio
>import Math.Matrix.Interface
>import Math.Number.DimensionalAnalysis
>import qualified GHC.TypeLits as TL
>import qualified GHC.TypeNats as TN
>import Data.Typeable
>import Data.Kind

>data Rat = Rat TN.Nat TN.Nat

>type OneRat  = 'Rat 1 1
>type ZeroRat = 'Rat 0 1

data RatioRep (a :: 'Rat x y) = RatioRep

data Rep :: Dim RatioRep -> * where

>-- In dimensional analysis implementation of
>-- "Barton&Nackman: Scientific and Engineering C++", the dimensions
>-- are checked in compile time. I think that's a mistake since it requires
>-- too much from the compiler (computation of rational arithmetic in types).
>-- Here's an approach (incomplete) that attempts to do that in Haskell's
>-- promoted types.

data Dim p = forall la lb wa wb ta tb ca cb tea teb lua lub sua sub. Dim {
   length_pow :: p ('Rat la lb),
   weight_pow :: p ('Rat wa wb),
   time_pow :: p ('Rat ta tb),
   current_pow :: p ('Rat ca cb),
   temperature_pow :: p ('Rat tea teb),
   luminosity_pow  :: p ('Rat lua lub),
   substance_pow :: p ('Rat sua sub) }

newtype Length = Length (Rep ('Dim OneRat ZeroRat ZeroRat ZeroRat ZeroRat ZeroRat ZeroRat))

newtype Length = Length (Rep ('Dim 1 1 0 1 0 1 0 1 0 1 0 1 0 1))
 deriving (Eq,Ord,Num,Show)

>newtype Length = Length (Quantity Double) deriving (Eq,Ord,Num,Show)
>newtype Weight = Weight (Quantity Double) deriving (Eq,Ord,Num,Show)
>newtype Speed  = Speed (Quantity Double) deriving (Eq,Ord,Num,Show)
>newtype Force  = Force (Quantity Double) deriving (Eq,Ord,Num,Show)
>newtype Time = Second (Quantity Double) deriving (Eq,Ord,Num,Show)
>newtype Current = Current (Quantity Double) deriving (Eq,Ord,Num,Show)
>newtype Temperature = Temperature (Quantity Double) deriving (Eq,Ord,Num,Show)
>newtype Luminosity = Luminosity (Quantity Double) deriving (Eq,Ord,Num,Show)
>newtype Substance = Substance (Quantity Double) deriving (Eq,Ord,Num,Show)

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

