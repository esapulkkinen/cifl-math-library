>{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
>module Math.Test.Common where
>import Test.QuickCheck
>import qualified Test.QuickCheck.Test
>import Test.HUnit
>import Math.Matrix.Interface
>import Math.Tools.Prop

>instance AssertionPredicable Property where
>   assertionPredicate x = quickCheckResult x >>= (return . Test.QuickCheck.Test.isSuccess)

>instance Test.HUnit.Testable Property where
>   test p = p ~? "property"
