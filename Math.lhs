>{-# LANGUAGE Safe #-}
>{-# OPTIONS_HADDOCK not-home #-}
>-- | 
>-- Module: Math
>-- Description: CIFL math libraries
>-- Copyright: (c) Esa Pulkkinen, 2018
>-- Maintainer: esa.pulkkinen@iki.fi
>-- Stability: experimental
>-- Portability: POSIX
>--
>-- Haskell libraries containing graphs, vector spaces, matrices,               
>--           real numbers.
>--               
>--           Haskell libraries for conversion of graphs to XML and DOT format
>--              
>--          See Github page at <https://github.com/esapulkkinen/cifl-math-library>.
>--             See documentation at <https://esapulkkinen.github.io/cifl-math-library/>.
>--               
>--          <https://esapulkkinen.github.io/cifl-math-library/dependencies-Tools.pdf Tools dependencies>
>--              
>--          <https://esapulkkinen.github.io/cifl-math-library/dependencies-Matrix.pdf Matrix dependencies>
>--
>--  <https://esapulkkinen.github.io/cifl-math-library/dependencies-Graph.pdf Graph dependencies>
>-- 
>--          <https://esapulkkinen.github.io/cifl-math-library/dependencies-Number.pdf Number dependencies>
>-- 
>--              <https://esapulkkinen.github.io/cifl-math-library/external-deps.pdf External dependencies>
>-- 
>--               <https://github.com/esapulkkinen/cifl-math-library/blob/master/COPYRIGHT COPYRIGHT>.
>-- 
>module Math (
>   module Math.Graph,
>   module Math.Matrix,
>   module Math.Number,
>   module Math.Tools
>   ) where
>import safe Math.Graph
>import safe Math.Matrix
>import safe Math.Number
>import safe Math.Tools

