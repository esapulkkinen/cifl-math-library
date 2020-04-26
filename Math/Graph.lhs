>{-# LANGUAGE Safe #-}
>{-# OPTIONS_HADDOCK not-home #-}
>-- |
>-- Module: Graph
>-- Description: Graph implementation
>-- Copyright: (c) Esa Pulkkinen, 2018
>-- Maintainer: esa.pulkkinen@iki.fi
>-- Stability: experimental
>-- Portability: POSIX
>-- 
>-- These exported modules implement graphs as specified in
>-- Lawvere, Rosebrugh: Sets for mathematics.
>-- 
>-- Compact representation of the graph is:
>-- 
>--   * digraphs: left and right actions of a three-element non-trivial monoid on endomorphisms of the two-element set
>-- 
>--   * graphs  : left and right actions of a four-element non-trivial monoid on endomorphisms of the two-element set
>-- 
>-- The graph representation is based on just single set which contains both
>-- vertices and edges. This means we have operation,
>-- 'Math.Graph.InGraphMonad.isVertexM', for determining whether an element
>-- of the set is a vertex or an edge.
>-- 
>-- For non-mathematicians, think of a database table with
>-- three or four fields, where fields are called
>-- 
>--   * id
>-- 
>--   * source
>-- 
>--   * target
>-- 
>--   * invert
>-- 
>-- The invert field is not present in a digraph.
>-- 
>-- The monoid structure provides basic properties of source and target actions.
>-- 
>-- prop> source . target == source
>-- 
>-- prop> target . source == target
>-- 
>-- prop> source . source == source
>-- 
>-- prop> target . target == target
>-- 
>-- Think of the actions in terms of state machines, from any element,
>-- we can always ask for source and target, but above equations mean
>-- that for vertices,
>-- 
>-- prop> isVertex v => source v == v
>-- 
>-- prop> isVertex v => target v == v
>-- 
>module Math.Graph (
>-- * operations for building graphs
>    module Math.Graph.Reversible,
>-- ** labeled graphs
>    module Math.Graph.Labeled,
>-- * graph algorithms
>    module Math.Graph.Algorithms,
>-- ** transformations between graphs
>    module Math.Graph.GraphMap,
>-- * string conversion for graphs
>    module Math.Graph.Show,
>-- * xml conversion for graphs
>    module Math.Graph.XML,
>-- * dot conversion for graphs
>    module Math.Graph.Dot,
>-- * monoid for id,source,target,invert operations
>    module Math.Graph.GraphMonoid,
>-- * interface for graph monad and arrows
>    module Math.Graph.Interface,
>-- * arrow for inspecting graph data
>    module Math.Graph.InGraphA,
>-- * monad for inspecting graph data
>    module Math.Graph.InGraphMonad,
>-- ** library for general actions of a monoid
>    module Math.Graph.Action
>  ) where
>import safe qualified Math.Graph.Action
>import safe Math.Graph.Reversible
>import safe Math.Graph.TransformationMonoid
>import safe Math.Graph.Labeled
>import safe Math.Graph.Algorithms
>import safe Math.Graph.GraphMap
>import safe Math.Graph.Show
>import safe Math.Graph.InGraphMonad
>import safe Math.Graph.GraphMonoid
>import safe Math.Graph.InGraphA
>import safe Math.Graph.Interface
>import safe Math.Graph.XML
>import safe Math.Graph.Dot
