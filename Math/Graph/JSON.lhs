>{-# LANGUAGE Trustworthy, CPP #-}
>{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
>{-# LANGUAGE OverloadedStrings #-}
>--  JSON uses Text.JSON package from <https://hackage.haskell.org/package/json>
>module Math.Graph.JSON where
>import Control.Arrow
>import qualified Text.JSON as JSON
>import Math.Graph.Reversible (Graph)
>import qualified Math.Graph.Reversible as G
>import Math.Graph.Interface
>import Math.Graph.InGraphMonad
>import Math.Graph.GraphMonoid
>import Math.Tools.I
>import Data.Set (Set)
>import qualified Data.Set as Set
>import qualified Data.Text as T hiding (concatMap)
>import Data.String

>jsonDigraphToJSON :: (Monad m, GraphMonoid mon Bool) => InGraphM mon JSON.JSValue m JSON.JSValue
>jsonDigraphToJSON = do edges <- linksM
>                       let edges' = JSON.makeObj $ map (first JSON.encodeStrict) $ concatMap jlist (Set.toList edges)
>                       vertices <- verticesM
>                       return $ JSON.makeObj [("vertices", JSON.showJSON vertices),
>                                              ("edges", JSON.showJSON edges')]
>     where jlist (e,f,t) = [(e,JSON.makeObj [("from", JSON.showJSON f),
>                                             ("to", JSON.showJSON t)])]

>textDigraphToJSON :: (Monad m, GraphMonoid mon Bool) => InGraphM mon T.Text m JSON.JSValue
>textDigraphToJSON = do edges <- linksM
>                       let edges' = JSON.makeObj $ map (first T.unpack) $ concatMap jlist (Set.toList edges)
>                       vertices <- verticesM
>                       return $ JSON.makeObj [("vertices", JSON.showJSON vertices),
>                                              ("edges", JSON.showJSON edges')]
>    where jlist (e,f,t) = [(e,JSON.makeObj [("from", JSON.showJSON f), ("to",JSON.showJSON t)])]

>digraphToJSON :: (Monad m, GraphMonoid mon Bool) => InGraphM mon String m JSON.JSValue
>digraphToJSON = do edges <- linksM
>                   let edges' = JSON.makeObj $ concatMap jlist (Set.toList edges)
>                   vertices <- verticesM
>                   return $ JSON.makeObj [("vertices",JSON.showJSON vertices),
>                                          ("edges",JSON.showJSON edges')]
>   where jlist (e,f,t) = [(e,JSON.makeObj [("from",JSON.showJSON f),
>                                            ("to",JSON.showJSON t)])]

>digraphToJSONString :: (Monad m, GraphMonoid mon Bool) => InGraphM mon String m String
>digraphToJSONString = digraphToJSON >>= (return . JSON.encode)

>digraphToJSONText :: (Monad m, GraphMonoid mon Bool) => InGraphM mon T.Text m T.Text
>digraphToJSONText = textDigraphToJSON >>= (return . T.pack . JSON.encode)
