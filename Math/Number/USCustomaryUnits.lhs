>{-# LANGUAGE Safe #-}
>module Math.Number.USCustomaryUnits where
>import safe Math.Number.DimensionalAnalysis
>import safe Math.Matrix.Interface
>
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

