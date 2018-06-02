>{-# LANGUAGE Safe #-}
>-- | These exported modules implement graphs as specified in
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
>-- | operations for building graphs
>    module Math.Graph.Reversible,
>-- | labeled graphs
>    module Math.Graph.Labeled,
>-- | graph algorithms
>    module Math.Graph.Algorithms,
>-- | transformations between graphs
>    module Math.Graph.GraphMap,
>-- | string conversion for graphs
>    module Math.Graph.Show,
>-- | xml conversion for graphs
>    module Math.Graph.XML,
>-- | dot conversion for graphs
>    module Math.Graph.Dot,
>-- | monoid for id,source,target,invert operations
>    module Math.Graph.GraphMonoid,
>-- | interface for graph monad and arrows
>    module Math.Graph.Interface,
>-- | arrow for inspecting graph data
>    module Math.Graph.InGraphA,
>-- | monad for inspecting graph data
>    module Math.Graph.InGraphMonad,
>-- | library for general actions of a monoid
>    module Math.Graph.Action)
>   where
>import Math.Graph.Action hiding (action, unyoneda)
>import Math.Graph.Reversible
>import Math.Graph.TransformationMonoid
>import Math.Graph.Labeled
>import Math.Graph.Algorithms
>import Math.Graph.GraphMap
>import Math.Graph.Show
>import Math.Graph.InGraphMonad
>import Math.Graph.GraphMonoid
>import Math.Graph.InGraphA
>import Math.Graph.Interface
>import Math.Graph.XML
>import Math.Graph.Dot
