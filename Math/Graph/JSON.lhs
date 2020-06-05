>{-# LANGUAGE Trustworthy, CPP #-}
>--  JSON uses Text.JSON package from <https://hackage.haskell.org/package/json>
>module Math.Graph.JSON where
>import qualified Text.JSON as JSON
>import Math.Graph.Reversible (Graph)
>import qualified Math.Graph.Reversible as G
>import Math.Graph.Interface
>import Math.Graph.InGraphMonad
>import Math.Graph.GraphMonoid
>import Data.Set (Set)
>import qualified Data.Set as Set

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
