>module Main where
>import System.Exit
>import Test.HUnit
>import qualified Math.Tools.Test.ToolsTests
>import qualified Math.Matrix.Test.MatrixTests
>import qualified Math.Number.Test.NumberTests
>main = do counts <- runTestTT tests
>          putStrLn (show counts)
>          if failures counts == 0 && errors counts == 0 then
>            exitSuccess
>           else
>            exitFailure

>tests = test [Math.Matrix.Test.MatrixTests.tests,
>              Math.Tools.Test.ToolsTests.tests,
>              Math.Number.Test.NumberTests.tests]
