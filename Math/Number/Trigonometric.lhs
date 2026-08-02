>{-# LANGUAGE TypeFamilies, StandaloneDeriving #-}
>module Math.Number.Trigonometric where
>import Data.Complex
>import Data.Ratio
>import GHC.Real
>import Math.Matrix.Interface
>import Math.Number.Interface

>data Trigonometric = TrigPower { trig_base :: Trigonometric, trig_power :: Rational }
>                   | TrigRational { trig_rational_value :: Rational }
>                   | TrigAngle { trig_fraction_of_circle :: Rational } -- 2*pi*q
>                   | TrigNegate { trig_negated_value :: Trigonometric }
>                   | TrigAbs { trig_absed_value :: Trigonometric }
>                   | TrigSum { trig_first_term :: Trigonometric, trig_second_term ::  Trigonometric }
>                   | TrigProduct { trig_first_factor :: Trigonometric, trig_second_factor :: Trigonometric }

>-- <https://en.wikipedia.org/wiki/List_of_trigonometric_identities>
>instance Floating Trigonometric where
>  pi = TrigAngle (1 % 2)
>  sqrt x = TrigPower x (1 % 2)
>  exp x = cosh x + sinh x
>  -- <https://en.wikipedia.org/wiki/Hyperbolic_functions>
>  cosh x = cos (imaginaryUnitTrig * x)
>  sinh x = - imaginaryUnitTrig * sin (imaginaryUnitTrig * x)
>  sin (TrigRational 0) = 0
>  sin (TrigSum a b) = sin a * cos b + cos a * sin b
>  sin (TrigNegate a) = TrigNegate (sin a)
>  sin (TrigAngle q) | Just r <- trigCis q = imagPart r
>                    | otherwise = error "not known to be exact"
>  sin (TrigProduct (TrigRational (0 :% _)) x) = 0
>  sin (TrigProduct (TrigRational (1 :% 1)) x) = sin x
>  sin (TrigProduct (TrigRational (n :% 1)) x) = -- multiple-angle formula
>    2*cos x*sin (TrigProduct (TrigRational (fromIntegral (n-1))) x)
>    - sin (TrigProduct (TrigRational (fromIntegral (n-2))) x)
>  sin (TrigProduct x z@(TrigRational (_ :% 1))) = sin (TrigProduct z x)
>  sin (TrigPower x (1 :% 1)) = sin x
>  sin (TrigPower x (n :% 1)) = sin (TrigProduct x (TrigPower x ((n-1) :% 1)))
>  cos (TrigRational 0) = 1
>  cos (TrigNegate a) = cos a
>  cos (TrigSum a b) = cos a * cos b - sin a * sin b
>  cos (TrigAngle q) | Just r <- trigCis q = realPart r
>                    | otherwise = error "not known to be exact"
>  cos (TrigProduct (TrigRational (0 :% _)) x) = 1
>  cos (TrigProduct (TrigRational (1 :% 1)) x) = cos x
>  cos (TrigProduct (TrigRational (n :% 1)) x) = -- multiple-angle formula
>    2*cos x*cos (TrigProduct (TrigRational (fromIntegral (n-1))) x)
>    - cos (TrigProduct (TrigRational (fromIntegral (n-2))) x)
>  cos (TrigProduct x z@(TrigRational (_ :% 1))) = cos (TrigProduct z x)
>  cos (TrigProduct (TrigPower (TrigNegate (TrigRational 1)) (1 :% 2)) x) = cosh x 
>  cos (TrigProduct x (TrigPower (TrigNegate (TrigRational 1)) (1 :% 2))) = cosh x
>  cos (TrigPower x (1 :% 1)) = cos x
>  cos (TrigPower x (n :% 1)) = cos (TrigProduct x (TrigPower x ((n-1) :% 1)))
>  tan z@(TrigAngle q) | Just r <- trigCis q = imagPart r / realPart r
>                      | otherwise = error "not known to be exact"
>  tan (TrigSum a b) = (tana + tanb) / (1 - tana * tanb)
>    where tana = tan a
>          tanb = tan b

>deriving instance Eq Trigonometric

>imaginaryUnit :: Complex Trigonometric
>imaginaryUnit = 0 :+ 1

>imaginaryUnitTrig :: Trigonometric
>imaginaryUnitTrig = TrigPower (negate 1) (1 % 2)

>evalTrigonometric :: (Floating a) => Trigonometric -> a
>evalTrigonometric (TrigPower x (a :% 2)) = sqrt (evalTrigonometric x ^ a)
>evalTrigonometric (TrigPower x (a :% b)) = nthroot (evalTrigonometric x) (fromIntegral b) ^ a
>evalTrigonometric (TrigRational r) = fromRational r
>evalTrigonometric (TrigAngle r) = 2*pi*fromRational r
>evalTrigonometric (TrigNegate v) = negate $ evalTrigonometric v
>evalTrigonometric (TrigAbs v) = abs $ evalTrigonometric v
>evalTrigonometric (TrigSum a b) = evalTrigonometric a + evalTrigonometric b
>evalTrigonometric (TrigProduct a b) = evalTrigonometric a * evalTrigonometric b

>evalComplexTrigonometric :: (Floating a) => Complex Trigonometric -> Complex a
>evalComplexTrigonometric (a :+ b) = evalTrigonometric a :+ evalTrigonometric b

>instance Ord Trigonometric where
>  compare x y = compare (evalTrigonometric x :: Double) (evalTrigonometric y :: Double)
>
>instance Real Trigonometric where
>  toRational x = toRational $ (evalTrigonometric x :: Double)

>instance Show Trigonometric where
>  showsPrec _ (TrigPower v (1 :% 2)) = showString "sqrt(" . showsPrec 8 v . showString ")"
>  showsPrec _ (TrigPower v (1 :% n)) = showString "root_" . shows n . showsPrec 2 v
>  showsPrec _ (TrigPower e (n :% 1)) = showsPrec 2 e . showString "^" . shows n
>  showsPrec _ (TrigPower v (n :% m)) = showString "(" . showsPrec 8 v . showString ")^" . shows (n :% m)
>  showsPrec _ (TrigRational (n :% 1)) = shows n
>  showsPrec _ (TrigRational q) = shows q
>  showsPrec _ (TrigAngle q) = showString "2π*" . showsPrec 6 q
>  showsPrec _ (TrigNegate v) = showChar '-' . showsPrec 6 v
>  showsPrec _ (TrigAbs v) = showString "abs(" . showsPrec 8 v . showString ")"
>  showsPrec p z@(TrigSum a (TrigNegate b)) | p >= 8 = shows a . showString " - " . shows b
>                                           | otherwise = showChar '(' . showsPrec 8 z . showChar ')'
>  showsPrec p z@(TrigSum a b) | p >= 8 = shows a . showString " + " . shows b
>                              | otherwise = showChar '(' . showsPrec 8 z . showChar ')'
>  showsPrec p z@(TrigProduct a b) | p >= 6 = showsPrec 7 a . showString " * " . showsPrec 7 b
>                                  | otherwise = showChar '(' . showsPrec 8 z . showChar ')'

>instance Num Trigonometric where
>  a + b = TrigSum a b
>  a - b = TrigSum a (TrigNegate b)
>  a * b = TrigProduct a b
>  abs = TrigAbs
>  signum x = x - TrigAbs x
>  negate = TrigNegate
>  fromInteger i = TrigRational (fromInteger i)

>instance Fractional Trigonometric where
>  recip x = TrigPower x (negate 1)
>  (TrigRational z) / (TrigRational z') = TrigRational (z / z')
>  a / b   = TrigProduct a (TrigPower b (negate 1))
>  fromRational = TrigRational

>instance RealFrac Trigonometric where
>  properFraction x = properFraction $ evalTrigonometric x
>  truncate x = truncate $ evalTrigonometric x
>  round x = round $ evalTrigonometric x
>  ceiling x = ceiling $ evalTrigonometric x
>  floor x = floor $ evalTrigonometric x
>
>instance RealFloat Trigonometric where
>  floatRadix = floatRadix . evalTrigonometric
>  floatDigits = floatDigits . evalTrigonometric
>  floatRange = floatRange . evalTrigonometric
>  exponent = exponent . evalTrigonometric
>  decodeFloat = decodeFloat . evalTrigonometric
>  encodeFloat = error "encodeFloat not supported for Trigonometric"
>  isNaN = isNaN . evalTrigonometric
>  isInfinite = isInfinite . evalTrigonometric
>  isDenormalized = isDenormalized . evalTrigonometric
>  isNegativeZero = isNegativeZero . evalTrigonometric
>  isIEEE = isIEEE . evalTrigonometric

>instance VectorSpace Trigonometric where
>  type Scalar Trigonometric = Rational
>  vzero = TrigRational vzero
>  vnegate = TrigNegate
>  a %+ b = TrigSum a b
>  q %* a = TrigProduct (TrigRational q) a

>circle :: Trigonometric
>circle = 2*pi

>sqrt2 :: Trigonometric
>sqrt2 = TrigPower (TrigRational 2) (1 % 2)

>sqrt3 :: Trigonometric
>sqrt3 = TrigPower (TrigRational 3) (1 % 2)

>sqrt5 :: Trigonometric
>sqrt5 = TrigPower (TrigRational 5) (1 % 2)

>sqrt17 :: Trigonometric
>sqrt17 = TrigPower (TrigRational 17) (1 % 2)

>doubleAngle :: Complex Trigonometric -> Complex Trigonometric
>doubleAngle (a :+ ai) = (1 - 2 * ai * ai) :+ (2 * ai * a)

>tripleAngle :: Complex Trigonometric -> Complex Trigonometric
>tripleAngle (a :+ ai) = (4 * a^3 - 3*a) :+ (3 * ai - 4 * ai^3)

>trigCisOfSum :: (MonadFail m) => Rational -> Rational -> m (Complex Trigonometric)
>trigCisOfSum q q' = do
>   (a :+ ai) <- trigCis q
>   (b :+ bi) <- trigCis q'
>   return $ (a*b %- ai*bi) :+ (ai*b %+ a*bi)

>trigCisOfSubtract :: (MonadFail m) => Rational -> Rational -> m (Complex Trigonometric)
>trigCisOfSubtract q q' = do
>   (a :+ ai) <- trigCis q
>   (b :+ bi) <- trigCis q'
>   return $ (a*b %+ ai*bi) :+ (ai*b %- a*bi)

>-- <https://en.wikipedia.org/wiki/Exact_trigonometric_values>
>-- this computes exact formula for many fractions of a circle.
>-- in particular, all multiples of 3 degrees are supported.
>-- the angle is expressed as trigCis q = cis (2*pi*q), i.e. 1.0 == 360 degrees, 0.0 = 0 degrees.
>trigCis :: (MonadFail m) => Rational -> m (Complex Trigonometric)
>trigCis 0 = return (1 :+ 0)
>trigCis (1 :% 2) = return $ negate 1 :+ 0 -- 180 degrees
>trigCis (1 :% 4) = return (0 :+ 1)  -- 90 degrees
>trigCis (1 :% 5) = doubleAngle <$> trigCis (1 % 10)
>trigCis (1 :% 6) = return $ TrigRational (1%2) :+ (1%2) %* sqrt3
>trigCis (1 :% 8)  = return $ (1%2) %* sqrt2 :+ (1%2) %* sqrt2
>trigCis (1 :% 10) = doubleAngle <$> trigCis (1 % 20)
>trigCis (1 :% 12) = return $ (1%2) %* sqrt3 :+ TrigRational (1%2)
>trigCis (2 :% 15) = trigCisOfSum (1 % 12) (1 % 20)
>trigCis (1 :% 15) = trigCisOfSubtract (1 % 6) (1 % 10)
>trigCis (1 :% 16) = return $ (1%2) %* sqrt (2 + sqrt2) :+ (1%2) %* sqrt (2 - sqrt2)
>-- 
>-- fermat prime 17, <https://en.wikipedia.org/wiki/Exact_trigonometric_values>
>--  <https://en.wikipedia.org/wiki/Heptadecagon>
>-- Originally found by Gauss.
>-- 
>trigCis (1 :% 17) = return $
>    ((1 % 16) %* (
>      negate 1 + sqrt17 + sqrt (34 - 2*sqrt17) +
>       2*sqrt (17 + 3*sqrt17 - sqrt (170 + 38*sqrt17))))
>    :+ (1 % 8) %* sqrt( 34 - 2*sqrt17 + sqrt(136 - 8 * sqrt17)
>               - sqrt(272 + 48 * sqrt17 + sqrt(43520 + 9728 * sqrt17)))
>trigCis (1 :% 20) = return $ (1%4) %* (sqrt2 * sqrt (5 + sqrt5))
>   :+ (1%4) %* (sqrt5 - 1)
>trigCis (5 :% 24) = trigCisOfSum (1 % 8) (1 % 12)
>trigCis (1 :% 24) = trigCisOfSubtract (1 % 8) (1 % 12)
>trigCis (7 :% 30) = trigCisOfSum    (3 % 20) (1 % 12)
>trigCis (1 :% 30)  = trigCisOfSubtract (1 % 12) (1 % 20)
>trigCis (9 :% 40)  = trigCisOfSum      (1 % 8) (1 % 10)
>trigCis (7 :% 40)  = trigCisOfSum      (1 % 8) (1 % 20)
>trigCis (3 :% 40)  = trigCisOfSubtract (1 % 8) (1 % 20)
>trigCis (1 :% 40)  = trigCisOfSubtract (1 % 8) (1 % 10)
>trigCis (13 :% 60) = trigCisOfSum      (1 % 6) (1 % 20)
>trigCis (11 :% 60) = trigCisOfSum      (1 % 10) (1 % 12)
>trigCis (7 :% 60)  = trigCisOfSubtract (1 % 6) (1 % 20)
>trigCis (1 :% 60)  = trigCisOfSubtract (1 % 10) (1 % 12)
>trigCis (1 :% 120) = trigCisOfSubtract (1 % 20) (1 % 24)
>trigCis (7 :% 120) = trigCisOfSubtract (1 % 10) (1 % 24)
>trigCis (11 :% 120) = trigCisOfSum     (1 % 20) (1 % 24)
>trigCis (13 :% 120) = trigCisOfSubtract (3 % 20) (1 % 24)
>trigCis (17 :% 120) = trigCisOfSum      (1 % 10) (1 % 24)
>trigCis (19 :% 120) = trigCisOfSubtract (5 % 24) (1 % 20)
>trigCis (23 :% 120) = trigCisOfSum      (3 % 20) (1 % 24)
>trigCis (27 :% 120) = trigCisOfSum      (1 % 8)  (1 % 10)
>trigCis (29 :% 120) = trigCisOfSum      (1 % 5)  (1 % 24)
>trigCis x@(a :% b)
>   -- double and triple angle formulas
>   | a `mod` 2 == 0 = doubleAngle <$> trigCis ((a `div` 2) % b)
>   | a `mod` 3 == 0 = tripleAngle <$> trigCis ((a `div` 3) % b)
>    -- de Moivre's formula: <https://en.wikipedia.org/wiki/De_Moivre's_formula>
>    -- notice this depends on Data.Complex for (^). Notice this expands the complexity easily.
>   | a /= 1 = (^a) <$> trigCis (1 % b)
>   -- the rest here are translations to different quadrants:
>   | x >= 1 = trigCis (x - 1)
>   | x >= (1 % 2) = do
>      (a :+ b) <- trigCis (x - (1 % 2))
>      return (negate a :+ negate b)
>   | x >= (1 % 4) = do
>      (a :+ b) <- trigCis (x - (1 % 4))
>      return $ negate b :+ a
>   | x < 0 = do
>      (a :+ b) <- trigCis (negate x)
>      return $ a :+ negate b
>   | otherwise = fail $ "not known to be exact:" ++ show x


>trigDftImpl :: (MonadFail m) => Integer -> [Complex Trigonometric] -> m (Complex Trigonometric)
>trigDftImpl k lst = do lst' <- mapM (mapper k) (zip lst [0.. n-1])
>                       return $ sum lst'
>  where n = fromIntegral (length lst)
>        mapper k (x,i) = fmap (x %*) $ trigCis ((i*k) % n)

>trigDft :: (MonadFail m) => [Complex Trigonometric] -> m [Complex Trigonometric]
>trigDft lst = mapM (`trigDftImpl` lst) [0..fromIntegral (length lst) - 1]

>trigDftValues :: (MonadFail m, Floating a) => [Complex Trigonometric] -> m [Complex a]
>trigDftValues lst = do xlst <- trigDft lst
>                       return $ map evalComplexTrigonometric xlst
