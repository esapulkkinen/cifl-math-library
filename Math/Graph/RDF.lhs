>{-# LANGUAGE OverloadedStrings, Trustworthy #-}
>module Math.Graph.RDF where
>import Data.RDF
>import safe qualified Data.Map as Map
>import safe qualified Data.Set as Set
>import safe qualified Math.Tools.Map as MapTools
>import safe Data.Map (Map)
>import safe Math.Graph.Interface
>import safe Math.Graph.GraphMonoid
>import safe Math.Graph.Reversible
>import safe Math.Graph.InGraphMonad
>import safe qualified Data.Text as T
>import safe Data.Text (Text)

>baseURL :: Text
>baseURL = "https://esapulkkinen.github.io/cifl-math-library/graph"
>graphNameURI :: Node
>graphNameURI = unode $ baseURL <> "#graphname"
>verticesURI :: Node
>verticesURI = unode $ baseURL <> "#vertices"
>linksURI :: Node
>linksURI = unode $ baseURL <> "#links"
>vertexURI :: Node
>vertexURI = unode $ baseURL <> "#vertex"
>linkURI :: Node
>linkURI = unode $ baseURL <> "#link"
>linkNameURI :: Node
>linkNameURI = unode $ baseURL <> "#linkname"
>vertexNameURI :: Node
>vertexNameURI = unode $ baseURL <> "#vertexname"
>sourcePropURI :: Node
>sourcePropURI = unode $ baseURL <> "#source"
>targetPropURI :: Node
>targetPropURI = unode $ baseURL <> "#target"
>inversePropURI :: Node
>inversePropURI = unode $ baseURL <> "#inverse"

>rdfReversibleLink :: Text -> Text -> Text -> Text -> Triples
>rdfReversibleLink n m v1 v2 | n == m = [
>   triple lnk linkNameURI (lnode $ plainL n),
>   triple lnk sourcePropURI v1',
>   triple lnk targetPropURI v2',
>   triple lnk inversePropURI lnk ]
>  where lnk = bnode n
>        v1' = unode v1
>        v2' = unode v2
>rdfReversibleLink n m v1 v2 =[
>   triple lnk linkNameURI (lnode $ plainL n),
>   triple lnk sourcePropURI v1',
>   triple lnk targetPropURI v2',
>   triple lnk inversePropURI lnk2,
>   triple lnk2 linkNameURI (lnode $ plainL m),
>   triple lnk2 sourcePropURI v2',
>   triple lnk2 targetPropURI v1',
>   triple lnk2 inversePropURI lnk]
> where lnk = bnode n
>       lnk2 = bnode m
>       v1' = unode v1
>       v2' = unode v2

>rdfLink :: Text -> Object -> Object -> Triples
>rdfLink n v1 v2 = [triple lnk linkNameURI (lnode $ plainL n),
>                   triple lnk sourcePropURI v1,
>                   triple lnk targetPropURI v2]
> where lnk = bnode n
> 
>rdfVertex :: Text -> Triples
>rdfVertex v = [triple (bnode v) vertexNameURI (lnode $ plainL v)]


>graphsToRDF :: (Monad m, ReversibleGraphMonoid mon)
>  => Text -> Map Text (Graph mon Text) -> m (Map Text (RDF TList))
>graphsToRDF graphURL m = do
>  graphTriples <- MapTools.mapByKeyM (\k g -> inGraphM g $ namedGraphToTriples k) m
>  let rdf k tp = mkRdf tp (Just $ BaseUrl $ graphURL <> "/" <> k)
>             (PrefixMappings $ Map.singleton "graph" baseURL)
>  return $ Map.mapWithKey rdf graphTriples

>namedGraphToTriples :: (Monad m, ReversibleGraphMonoid mon)
>  => Text -> InGraphM mon Text m Triples
>namedGraphToTriples graphname = do
>   let gnode = bnode graphname
>   triples <- graphToTriples
>   vert <- gvertices
>   let vertTriples = Set.map (\v -> triple gnode verticesURI (unode v)) vert
>   links <- greversibleLinks
>   let linkTriples = Set.map (\((l,_),(_,_)) -> triple gnode linksURI (unode l)) links
>   return $ triples ++ (Set.toList $ vertTriples `Set.union` linkTriples)

>graphToTriples :: (Monad m, ReversibleGraphMonoid mon)
>  => InGraphM mon Text m Triples
>graphToTriples = do
>   v <- gvertices
>   lnks <- greversibleLinks
>   let links = concat $ Set.toList $
>         Set.map (\ ((n,m),(v',w')) -> rdfReversibleLink n m v' w') lnks
>   let verts = concat $ Set.toList (Set.map rdfVertex v)
>   return $ links ++ verts

>graphToRDF :: (Monad m, ReversibleGraphMonoid mon)
>  => Text -> InGraphM mon Text m (RDF TList)
>graphToRDF graphURL = do
>   triples <- graphToTriples
>   let rdf = mkRdf triples (Just $ BaseUrl $ graphURL)
>             (PrefixMappings $ Map.singleton "graph" baseURL)
>   return $ rdf

>rdfToGraph :: (Monad m) => RDF TList -> Node -> m (Graph Four Node)
>rdfToGraph rdf graph = do
>      let vertices = query rdf (Just graph) (Just vertexURI) Nothing
>      let links = query rdf (Just graph) (Just linkURI) Nothing
>      convertedVertices <- mapM (convertVertex rdf) vertices
>      convertedLinks <- mapM (convertLink rdf) links
>      return $ reversibleEdgesG convertedLinks
>               `unionG` verticesG convertedVertices

>type Graphs = Map Node (Graph Four Node)

>rdfToGraphs :: (Monad m) => RDF TList -> m Graphs
>rdfToGraphs rdf = do
>       let graphs = query rdf Nothing (Just graphNameURI) Nothing
>       graphlist <- mapM handleTriple graphs
>       return $ Map.fromList graphlist
>  where handleTriple t = do
>           g <- rdfToGraph rdf (subjectOf t)
>           return (objectOf t, g)

>rdfFileToGraphs :: (RdfParser p) => p -> String -> IO Graphs
>rdfFileToGraphs parser filename = do
>   rdfOrFail <- parseFile parser filename
>   case rdfOrFail of
>     Left pf -> fail (show pf)
>     Right rdf -> rdfToGraphs rdf


>convertVertex :: (Monad m) => RDF TList -> Triple -> m Node
>convertVertex rdf (Triple subj pred obj)
> = getUniqueObject (query rdf (Just obj) (Just vertexNameURI) Nothing)

>convertLink :: (Monad m) => RDF TList -> Triple -> m ((Node,Node),(Node,Node))
>convertLink rdf (Triple subj pred obj) = do
>     linkname <- getUniqueObject $
>               query rdf (Just obj) (Just linkNameURI) Nothing
>     source   <- getUniqueObject $
>               query rdf (Just obj) (Just sourcePropURI) Nothing
>     target   <- getUniqueObject $
>               query rdf (Just obj) (Just targetPropURI) Nothing
>     inverse  <- getUniqueObject $
>               query rdf (Just obj) (Just inversePropURI) Nothing
>     inversename <- getUniqueObject $
>               query rdf (Just inverse) (Just linkNameURI) Nothing
>     return $ ((linkname,inversename),(source,target))

>getUniqueObject :: (Monad m) => [Triple] -> m Node
>getUniqueObject [t] = return $ objectOf t
>getUniqueObject _ = fail "uniqueness constraint violated"

