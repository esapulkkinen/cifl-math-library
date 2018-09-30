>{-# LANGUAGE Trustworthy, CPP #-}
>-- | This module provides conversions from graphs to Gxl format using HaXML.
>-- 
>--  HaXML is (c) Malcolm Wallace and licensed under LGPL.
>--  The Math.Graph.XML.GXL is generated using HaXML.
>--
>--  To enable HaXML support, please use -fHaXML option to cabal.
>--  (WARNING: since some time ago, HaXml has conflicted with base package).
>--  Until those are resolved, compiling with HaXml may be difficult.
>-- 
>--  JSON uses Text.JSON package from <https://hackage.haskell.org/package/json>
>-- 
>--  Please see HaXML web page, <https://hackage.haskell.org/package/HaXml>
>--  and GXL web page <http://www.gupro.de/GXL/> for further details about
>--  these packages.
>module Math.Graph.XML where
>import Data.Set (Set)
>import Data.Map (Map)
>import qualified Data.Set as Set
>import qualified Data.Map as Map
>

#ifdef WITH_HAXML

>import Text.XML.HaXml.Pretty
>import Text.XML.HaXml.Namespaces
>import Text.XML.HaXml.XmlContent
>import Text.XML.HaXml.Types
>import Text.XML.HaXml.OneOfN

#endif

>import qualified Text.JSON as JSON
>import Text.PrettyPrint
>import Math.Graph.XML.GXL
>import Math.Graph.Reversible (Graph)
>import qualified Math.Graph.Reversible as G
>import Math.Graph.Interface
>import Math.Graph.InGraphMonad
>import Math.Graph.GraphMonoid

#ifdef WITH_HAXML

>digraphToXMLString :: (Monad m, GraphMonoid mon) => G.Graph mon String -> m String
>digraphToXMLString g = do
>    v <- inGraphM g digraphToXML
>    let d = toXml False v
>    let d' = resolveAllNames qualify d
>    return $ render $ document d'

>bidirectionalGraphToXMLString :: (Monad m, ReversibleGraphMonoid mon) => G.Graph mon String -> m String
>bidirectionalGraphToXMLString g = do
>    v <- inGraphM g bidirectionalGraphToXML
>    return $ showXml False v

#endif

>digraphToJSON :: (Monad m, GraphMonoid mon) => InGraphM mon String m JSON.JSValue
>digraphToJSON = do edges <- linksM
>                   let edges' = JSON.makeObj $ concatMap 
>                        (\(e,f,t) ->  [(e,JSON.makeObj [("from",JSON.showJSON f),
>                                                        ("to",JSON.showJSON t)])])
>                        (Set.toList edges)
>                   vertices <- verticesM
>                   return $ JSON.makeObj [("vertices",JSON.showJSON vertices),
>                                          ("edges",JSON.showJSON edges')]

>digraphToJSONString :: (Monad m, GraphMonoid mon) => InGraphM mon String m String
>digraphToJSONString = digraphToJSON >>= (return . JSON.encode)

#ifdef WITH_HAXML

>digraphToXML :: (Monad m, GraphMonoid mon) => InGraphM mon String m Gxl
>digraphToXML = do edges <- edgesM
>                  edges' <- mapM make_edge (Set.toList edges)
>                  vertices <- verticesM
>                  vertices' <- mapM make_node (Set.toList vertices)
>                  return $ gxl (edges' ++ vertices')
>   where gxl els = Gxl gxl_attrs [gr els]
>         gxl_attrs = Gxl_Attrs (NonDefault "")
>         gr  els = Graph graph_attrs Nothing [] els
>         graph_attrs = Graph_Attrs "graph-id" Nothing (NonDefault Graph_edgeids_true) (NonDefault Graph_hypergraph_false) (NonDefault Graph_edgemode_directed)
>         make_node v = return $ OneOf3 $ Node (Node_Attrs v) Nothing [] []
>         make_edge e = do s <- sourceM e
>                          t <- targetM e
>                          return $ TwoOf3 $ Edge (edge_attrs e s t) Nothing [label e] []
>         edge_attrs e s t = Edge_Attrs (Just e) s t Nothing Nothing (Just Edge_isdirected_true)
>         label e = Attr (Attr_Attrs Nothing "label" Nothing) [] (FiveOf10 (AString e))

>bidirectionalGraphToXML :: (Monad m, ReversibleGraphMonoid mon) => InGraphM mon String m Gxl
>bidirectionalGraphToXML = do
>   links <- reversibleLinksM
>   vertices <- verticesM
>   edges' <- mapM make_edge (Set.toList links)
>   vertices' <- mapM make_node (Set.toList vertices)
>   return $ gxl (edges' ++ vertices')
> where gxl els = Gxl gxl_attrs [gr els]
>       gxl_attrs = Gxl_Attrs (NonDefault "")
>       gr els = Graph graph_attrs Nothing [] els
>       graph_attrs = Graph_Attrs "graph-id" Nothing (NonDefault Graph_edgeids_true) (NonDefault Graph_hypergraph_false) (NonDefault Graph_edgemode_undirected)
>       make_node v = return $ OneOf3 $ Node (Node_Attrs v) Nothing [] []
>       make_edge ((efwd,ebck),(s,t)) = do
>          return $ TwoOf3 $ Edge (edge_attrs efwd s t) Nothing [label (efwd ++ "/" ++ ebck)] []
>       edge_attrs e s t = Edge_Attrs (Just e) s t Nothing Nothing
>                               (Just Edge_isdirected_false)
>       label e = Attr (Attr_Attrs Nothing "label" Nothing) []
>                      (FiveOf10 (AString e))

#endif
