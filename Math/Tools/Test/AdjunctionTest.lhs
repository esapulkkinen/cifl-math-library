>{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
>module Math.Tools.Test.AdjunctionTest where
>import Test.HUnit
>import Test.QuickCheck
>import Math.Tools.Adjunction
>
>instance Show (a -> b) where
>   show x = "<<function>>"
>
>equal_functions :: (Show a, Arbitrary a, Eq b, Show b) => (a -> b) -> (a -> b) -> Property
>equal_functions f g = forAll arbitrary $ \a -> f a === g a
>
>prop_unit_arrow = unit () `equal_functions` \(x :: Int) -> (x,())
>prop_counit_arrow = counit ("a",(++"b")) === "ab"


>$(return [])
>qcTests = $quickCheckAll
> 
>tests = test [
>    "properties" ~: qcTests  
>  ]
