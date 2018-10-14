{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide, prune #-}
module Math.Graph.XML.GXL where

#ifdef WITH_HAXML

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Types
import Text.XML.HaXml.OneOfN


{-Type decls-}

data Gxl = Gxl Gxl_Attrs [Graph]
         deriving (Eq,Show)
data Gxl_Attrs = Gxl_Attrs
    { gxlXmlns'xlink :: (Defaultable String)
    } deriving (Eq,Show)
data Type = Type
    { typeXlink'type :: (Defaultable Type_xlink'type)
    , typeXlink'href :: String
    } deriving (Eq,Show)
data Type_xlink'type = Type_xlink'type_simple
                     deriving (Eq,Show)
data Graph = Graph Graph_Attrs (Maybe Type) [Attr]
                   [(OneOf3 Node Edge Rel)]
           deriving (Eq,Show)
data Graph_Attrs = Graph_Attrs
    { graphId :: String
    , graphRole :: (Maybe String)
    , graphEdgeids :: (Defaultable Graph_edgeids)
    , graphHypergraph :: (Defaultable Graph_hypergraph)
    , graphEdgemode :: (Defaultable Graph_edgemode)
    } deriving (Eq,Show)
data Graph_edgeids = Graph_edgeids_true  |  Graph_edgeids_false
                   deriving (Eq,Show)
data Graph_hypergraph = Graph_hypergraph_true  | 
                        Graph_hypergraph_false
                      deriving (Eq,Show)
data Graph_edgemode = Graph_edgemode_directed  | 
                      Graph_edgemode_undirected  |  Graph_edgemode_defaultdirected  | 
                      Graph_edgemode_defaultundirected
                    deriving (Eq,Show)
data Node = Node Node_Attrs (Maybe Type) [Attr] [Graph]
          deriving (Eq,Show)
data Node_Attrs = Node_Attrs
    { nodeId :: String
    } deriving (Eq,Show)
data Edge = Edge Edge_Attrs (Maybe Type) [Attr] [Graph]
          deriving (Eq,Show)
data Edge_Attrs = Edge_Attrs
    { edgeId :: (Maybe String)
    , edgeFrom :: String
    , edgeTo :: String
    , edgeFromorder :: (Maybe String)
    , edgeToorder :: (Maybe String)
    , edgeIsdirected :: (Maybe Edge_isdirected)
    } deriving (Eq,Show)
data Edge_isdirected = Edge_isdirected_true  | 
                       Edge_isdirected_false
                     deriving (Eq,Show)
data Rel = Rel Rel_Attrs (Maybe Type) [Attr] [Graph] [Relend]
         deriving (Eq,Show)
data Rel_Attrs = Rel_Attrs
    { relId :: (Maybe String)
    , relIsdirected :: (Maybe Rel_isdirected)
    } deriving (Eq,Show)
data Rel_isdirected = Rel_isdirected_true  |  Rel_isdirected_false
                    deriving (Eq,Show)
data Relend = Relend Relend_Attrs [Attr]
            deriving (Eq,Show)
data Relend_Attrs = Relend_Attrs
    { relendTarget :: String
    , relendRole :: (Maybe String)
    , relendDirection :: (Maybe Relend_direction)
    , relendStartorder :: (Maybe String)
    , relendEndorder :: (Maybe String)
    } deriving (Eq,Show)
data Relend_direction = Relend_direction_in  | 
                        Relend_direction_out  |  Relend_direction_none
                      deriving (Eq,Show)
data Attr = Attr Attr_Attrs [Attr]
                 (OneOf10 Locator ABool AInt AFloat AString AEnum Sequ Set Bag Tup)
          deriving (Eq,Show)
data Attr_Attrs = Attr_Attrs
    { attrId :: (Maybe String)
    , attrName :: String
    , attrKind :: (Maybe String)
    } deriving (Eq,Show)
data Locator = Locator
    { locatorXlink'type :: (Defaultable Locator_xlink'type)
    , locatorXlink'href :: String
    } deriving (Eq,Show)
data Locator_xlink'type = Locator_xlink'type_simple
                        deriving (Eq,Show)
newtype ABool = ABool String deriving (Eq,Show)
newtype AInt = AInt String deriving (Eq,Show)
newtype AFloat = AFloat String deriving (Eq,Show)
newtype AString = AString String deriving (Eq,Show)
newtype AEnum = AEnum String deriving (Eq,Show)
newtype Sequ = Sequ [Sequ_] deriving (Eq,Show)
data Sequ_ = Sequ_Locator Locator
           | Sequ_ABool ABool
           | Sequ_AInt AInt
           | Sequ_AFloat AFloat
           | Sequ_AString AString
           | Sequ_AEnum AEnum
           | Sequ_Sequ Sequ
           | Sequ_Set Set
           | Sequ_Bag Bag
           | Sequ_Tup Tup
           deriving (Eq,Show)
newtype Set = Set [Set_] deriving (Eq,Show)
data Set_ = Set_Locator Locator
          | Set_ABool ABool
          | Set_AInt AInt
          | Set_AFloat AFloat
          | Set_AString AString
          | Set_AEnum AEnum
          | Set_Sequ Sequ
          | Set_Set Set
          | Set_Bag Bag
          | Set_Tup Tup
          deriving (Eq,Show)
newtype Bag = Bag [Bag_] deriving (Eq,Show)
data Bag_ = Bag_Locator Locator
          | Bag_ABool ABool
          | Bag_AInt AInt
          | Bag_AFloat AFloat
          | Bag_AString AString
          | Bag_AEnum AEnum
          | Bag_Sequ Sequ
          | Bag_Set Set
          | Bag_Bag Bag
          | Bag_Tup Tup
          deriving (Eq,Show)
newtype Tup = Tup [Tup_] deriving (Eq,Show)
data Tup_ = Tup_Locator Locator
          | Tup_ABool ABool
          | Tup_AInt AInt
          | Tup_AFloat AFloat
          | Tup_AString AString
          | Tup_AEnum AEnum
          | Tup_Sequ Sequ
          | Tup_Set Set
          | Tup_Bag Bag
          | Tup_Tup Tup
          deriving (Eq,Show)


{-Instance decls-}

instance HTypeable Gxl where
    toHType x = Defined "gxl" [] []
instance XmlContent Gxl where
    toContents (Gxl as a) =
        [CElem (Elem (N "gxl") (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["gxl"]
        ; interior e $ return (Gxl (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <gxl>, "++)
instance XmlAttributes Gxl_Attrs where
    fromAttrs as =
        Gxl_Attrs
          { gxlXmlns'xlink = defaultA fromAttrToStr "http://www.w3.org/1999/xlink" "xmlns:xlink" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns:xlink" (gxlXmlns'xlink v)
        ]

instance HTypeable Type where
    toHType x = Defined "type" [] []
instance XmlContent Type where
    toContents as =
        [CElem (Elem (N "type") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["type"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <type>, "++)
instance XmlAttributes Type where
    fromAttrs as =
        Type
          { typeXlink'type = defaultA fromAttrToTyp Type_xlink'type_simple "xlink:type" as
          , typeXlink'href = definiteA fromAttrToStr "type" "xlink:href" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "xlink:type" (typeXlink'type v)
        , toAttrFrStr "xlink:href" (typeXlink'href v)
        ]

instance XmlAttrType Type_xlink'type where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "simple" = Just Type_xlink'type_simple
            translate _ = Nothing
    toAttrFrTyp n Type_xlink'type_simple = Just (N n, str2attr "simple")

instance HTypeable Graph where
    toHType x = Defined "graph" [] []
instance XmlContent Graph where
    toContents (Graph as a b c) =
        [CElem (Elem (N "graph") (toAttrs as) (maybe [] toContents a ++
                                               concatMap toContents b ++
                                               concatMap toContents c)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["graph"]
        ; interior e $ return (Graph (fromAttrs as))
                       `apply` optional parseContents `apply` many parseContents
                       `apply` many parseContents
        } `adjustErr` ("in <graph>, "++)
instance XmlAttributes Graph_Attrs where
    fromAttrs as =
        Graph_Attrs
          { graphId = definiteA fromAttrToStr "graph" "id" as
          , graphRole = possibleA fromAttrToStr "role" as
          , graphEdgeids = defaultA fromAttrToTyp Graph_edgeids_false "edgeids" as
          , graphHypergraph = defaultA fromAttrToTyp Graph_hypergraph_false "hypergraph" as
          , graphEdgemode = defaultA fromAttrToTyp Graph_edgemode_directed "edgemode" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (graphId v)
        , maybeToAttr toAttrFrStr "role" (graphRole v)
        , defaultToAttr toAttrFrTyp "edgeids" (graphEdgeids v)
        , defaultToAttr toAttrFrTyp "hypergraph" (graphHypergraph v)
        , defaultToAttr toAttrFrTyp "edgemode" (graphEdgemode v)
        ]

instance XmlAttrType Graph_edgeids where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "true" = Just Graph_edgeids_true
            translate "false" = Just Graph_edgeids_false
            translate _ = Nothing
    toAttrFrTyp n Graph_edgeids_true = Just (N n, str2attr "true")
    toAttrFrTyp n Graph_edgeids_false = Just (N n, str2attr "false")

instance XmlAttrType Graph_hypergraph where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "true" = Just Graph_hypergraph_true
            translate "false" = Just Graph_hypergraph_false
            translate _ = Nothing
    toAttrFrTyp n Graph_hypergraph_true = Just (N n, str2attr "true")
    toAttrFrTyp n Graph_hypergraph_false = Just (N n, str2attr "false")

instance XmlAttrType Graph_edgemode where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "directed" = Just Graph_edgemode_directed
            translate "undirected" = Just Graph_edgemode_undirected
            translate "defaultdirected" = Just Graph_edgemode_defaultdirected
            translate "defaultundirected" = Just Graph_edgemode_defaultundirected
            translate _ = Nothing
    toAttrFrTyp n Graph_edgemode_directed = Just (N n, str2attr "directed")
    toAttrFrTyp n Graph_edgemode_undirected = Just (N n, str2attr "undirected")
    toAttrFrTyp n Graph_edgemode_defaultdirected = Just (N n, str2attr "defaultdirected")
    toAttrFrTyp n Graph_edgemode_defaultundirected = Just (N n, str2attr "defaultundirected")

instance HTypeable Node where
    toHType x = Defined "node" [] []
instance XmlContent Node where
    toContents (Node as a b c) =
        [CElem (Elem (N "node") (toAttrs as) (maybe [] toContents a ++
                                              concatMap toContents b ++ concatMap toContents c)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["node"]
        ; interior e $ return (Node (fromAttrs as))
                       `apply` optional parseContents `apply` many parseContents
                       `apply` many parseContents
        } `adjustErr` ("in <node>, "++)
instance XmlAttributes Node_Attrs where
    fromAttrs as =
        Node_Attrs
          { nodeId = definiteA fromAttrToStr "node" "id" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (nodeId v)
        ]

instance HTypeable Edge where
    toHType x = Defined "edge" [] []
instance XmlContent Edge where
    toContents (Edge as a b c) =
        [CElem (Elem (N "edge") (toAttrs as) (maybe [] toContents a ++
                                              concatMap toContents b ++ concatMap toContents c)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["edge"]
        ; interior e $ return (Edge (fromAttrs as))
                       `apply` optional parseContents `apply` many parseContents
                       `apply` many parseContents
        } `adjustErr` ("in <edge>, "++)
instance XmlAttributes Edge_Attrs where
    fromAttrs as =
        Edge_Attrs
          { edgeId = possibleA fromAttrToStr "id" as
          , edgeFrom = definiteA fromAttrToStr "edge" "from" as
          , edgeTo = definiteA fromAttrToStr "edge" "to" as
          , edgeFromorder = possibleA fromAttrToStr "fromorder" as
          , edgeToorder = possibleA fromAttrToStr "toorder" as
          , edgeIsdirected = possibleA fromAttrToTyp "isdirected" as
          }
    toAttrs v = catMaybes 
        [ maybeToAttr toAttrFrStr "id" (edgeId v)
        , toAttrFrStr "from" (edgeFrom v)
        , toAttrFrStr "to" (edgeTo v)
        , maybeToAttr toAttrFrStr "fromorder" (edgeFromorder v)
        , maybeToAttr toAttrFrStr "toorder" (edgeToorder v)
        , maybeToAttr toAttrFrTyp "isdirected" (edgeIsdirected v)
        ]

instance XmlAttrType Edge_isdirected where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "true" = Just Edge_isdirected_true
            translate "false" = Just Edge_isdirected_false
            translate _ = Nothing
    toAttrFrTyp n Edge_isdirected_true = Just (N n, str2attr "true")
    toAttrFrTyp n Edge_isdirected_false = Just (N n, str2attr "false")

instance HTypeable Rel where
    toHType x = Defined "rel" [] []
instance XmlContent Rel where
    toContents (Rel as a b c d) =
        [CElem (Elem (N "rel") (toAttrs as) (maybe [] toContents a ++
                                             concatMap toContents b ++ concatMap toContents c ++
                                             concatMap toContents d)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["rel"]
        ; interior e $ return (Rel (fromAttrs as))
                       `apply` optional parseContents `apply` many parseContents
                       `apply` many parseContents `apply` many parseContents
        } `adjustErr` ("in <rel>, "++)
instance XmlAttributes Rel_Attrs where
    fromAttrs as =
        Rel_Attrs
          { relId = possibleA fromAttrToStr "id" as
          , relIsdirected = possibleA fromAttrToTyp "isdirected" as
          }
    toAttrs v = catMaybes 
        [ maybeToAttr toAttrFrStr "id" (relId v)
        , maybeToAttr toAttrFrTyp "isdirected" (relIsdirected v)
        ]

instance XmlAttrType Rel_isdirected where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "true" = Just Rel_isdirected_true
            translate "false" = Just Rel_isdirected_false
            translate _ = Nothing
    toAttrFrTyp n Rel_isdirected_true = Just (N n, str2attr "true")
    toAttrFrTyp n Rel_isdirected_false = Just (N n, str2attr "false")

instance HTypeable Relend where
    toHType x = Defined "relend" [] []
instance XmlContent Relend where
    toContents (Relend as a) =
        [CElem (Elem (N "relend") (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["relend"]
        ; interior e $ return (Relend (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <relend>, "++)
instance XmlAttributes Relend_Attrs where
    fromAttrs as =
        Relend_Attrs
          { relendTarget = definiteA fromAttrToStr "relend" "target" as
          , relendRole = possibleA fromAttrToStr "role" as
          , relendDirection = possibleA fromAttrToTyp "direction" as
          , relendStartorder = possibleA fromAttrToStr "startorder" as
          , relendEndorder = possibleA fromAttrToStr "endorder" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "target" (relendTarget v)
        , maybeToAttr toAttrFrStr "role" (relendRole v)
        , maybeToAttr toAttrFrTyp "direction" (relendDirection v)
        , maybeToAttr toAttrFrStr "startorder" (relendStartorder v)
        , maybeToAttr toAttrFrStr "endorder" (relendEndorder v)
        ]

instance XmlAttrType Relend_direction where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "in" = Just Relend_direction_in
            translate "out" = Just Relend_direction_out
            translate "none" = Just Relend_direction_none
            translate _ = Nothing
    toAttrFrTyp n Relend_direction_in = Just (N n, str2attr "in")
    toAttrFrTyp n Relend_direction_out = Just (N n, str2attr "out")
    toAttrFrTyp n Relend_direction_none = Just (N n, str2attr "none")

instance HTypeable Attr where
    toHType x = Defined "attr" [] []
instance XmlContent Attr where
    toContents (Attr as a b) =
        [CElem (Elem (N "attr") (toAttrs as) (concatMap toContents a ++
                                              toContents b)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["attr"]
        ; interior e $ return (Attr (fromAttrs as))
                       `apply` many parseContents `apply` parseContents
        } `adjustErr` ("in <attr>, "++)
instance XmlAttributes Attr_Attrs where
    fromAttrs as =
        Attr_Attrs
          { attrId = possibleA fromAttrToStr "id" as
          , attrName = definiteA fromAttrToStr "attr" "name" as
          , attrKind = possibleA fromAttrToStr "kind" as
          }
    toAttrs v = catMaybes 
        [ maybeToAttr toAttrFrStr "id" (attrId v)
        , toAttrFrStr "name" (attrName v)
        , maybeToAttr toAttrFrStr "kind" (attrKind v)
        ]

instance HTypeable Locator where
    toHType x = Defined "locator" [] []
instance XmlContent Locator where
    toContents as =
        [CElem (Elem (N "locator") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["locator"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <locator>, "++)
instance XmlAttributes Locator where
    fromAttrs as =
        Locator
          { locatorXlink'type = defaultA fromAttrToTyp Locator_xlink'type_simple "xlink:type" as
          , locatorXlink'href = definiteA fromAttrToStr "locator" "xlink:href" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "xlink:type" (locatorXlink'type v)
        , toAttrFrStr "xlink:href" (locatorXlink'href v)
        ]

instance XmlAttrType Locator_xlink'type where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "simple" = Just Locator_xlink'type_simple
            translate _ = Nothing
    toAttrFrTyp n Locator_xlink'type_simple = Just (N n, str2attr "simple")

instance HTypeable ABool where
    toHType x = Defined "bool" [] []
instance XmlContent ABool where
    toContents (ABool a) =
        [CElem (Elem (N "bool") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["bool"]
        ; interior e $ return (ABool) `apply` (text `onFail` return "")
        } `adjustErr` ("in <bool>, "++)

instance HTypeable AInt where
    toHType x = Defined "int" [] []
instance XmlContent AInt where
    toContents (AInt a) =
        [CElem (Elem (N "int") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["int"]
        ; interior e $ return (AInt) `apply` (text `onFail` return "")
        } `adjustErr` ("in <int>, "++)

instance HTypeable AFloat where
    toHType x = Defined "float" [] []
instance XmlContent AFloat where
    toContents (AFloat a) =
        [CElem (Elem (N "float") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["float"]
        ; interior e $ return (AFloat) `apply` (text `onFail` return "")
        } `adjustErr` ("in <float>, "++)

instance HTypeable AString where
    toHType x = Defined "string" [] []
instance XmlContent AString where
    toContents (AString a) =
        [CElem (Elem (N "string") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["string"]
        ; interior e $ return (AString) `apply` (text `onFail` return "")
        } `adjustErr` ("in <string>, "++)

instance HTypeable AEnum where
    toHType x = Defined "enum" [] []
instance XmlContent AEnum where
    toContents (AEnum a) =
        [CElem (Elem (N "enum") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["enum"]
        ; interior e $ return (AEnum) `apply` (text `onFail` return "")
        } `adjustErr` ("in <enum>, "++)

instance HTypeable Sequ where
    toHType x = Defined "sequ" [] []
instance XmlContent Sequ where
    toContents (Sequ a) =
        [CElem (Elem (N "sequ") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["sequ"]
        ; interior e $ return (Sequ) `apply` many parseContents
        } `adjustErr` ("in <sequ>, "++)

instance HTypeable Sequ_ where
    toHType x = Defined "sequ" [] []
instance XmlContent Sequ_ where
    toContents (Sequ_Locator a) = toContents a
    toContents (Sequ_ABool a) = toContents a
    toContents (Sequ_AInt a) = toContents a
    toContents (Sequ_AFloat a) = toContents a
    toContents (Sequ_AString a) = toContents a
    toContents (Sequ_AEnum a) = toContents a
    toContents (Sequ_Sequ a) = toContents a
    toContents (Sequ_Set a) = toContents a
    toContents (Sequ_Bag a) = toContents a
    toContents (Sequ_Tup a) = toContents a
    parseContents = oneOf
        [ return (Sequ_Locator) `apply` parseContents
        , return (Sequ_ABool) `apply` parseContents
        , return (Sequ_AInt) `apply` parseContents
        , return (Sequ_AFloat) `apply` parseContents
        , return (Sequ_AString) `apply` parseContents
        , return (Sequ_AEnum) `apply` parseContents
        , return (Sequ_Sequ) `apply` parseContents
        , return (Sequ_Set) `apply` parseContents
        , return (Sequ_Bag) `apply` parseContents
        , return (Sequ_Tup) `apply` parseContents
        ] `adjustErr` ("in <sequ>, "++)

instance HTypeable Set where
    toHType x = Defined "set" [] []
instance XmlContent Set where
    toContents (Set a) =
        [CElem (Elem (N "set") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["set"]
        ; interior e $ return (Set) `apply` many parseContents
        } `adjustErr` ("in <set>, "++)

instance HTypeable Set_ where
    toHType x = Defined "set" [] []
instance XmlContent Set_ where
    toContents (Set_Locator a) = toContents a
    toContents (Set_ABool a) = toContents a
    toContents (Set_AInt a) = toContents a
    toContents (Set_AFloat a) = toContents a
    toContents (Set_AString a) = toContents a
    toContents (Set_AEnum a) = toContents a
    toContents (Set_Sequ a) = toContents a
    toContents (Set_Set a) = toContents a
    toContents (Set_Bag a) = toContents a
    toContents (Set_Tup a) = toContents a
    parseContents = oneOf
        [ return (Set_Locator) `apply` parseContents
        , return (Set_ABool) `apply` parseContents
        , return (Set_AInt) `apply` parseContents
        , return (Set_AFloat) `apply` parseContents
        , return (Set_AString) `apply` parseContents
        , return (Set_AEnum) `apply` parseContents
        , return (Set_Sequ) `apply` parseContents
        , return (Set_Set) `apply` parseContents
        , return (Set_Bag) `apply` parseContents
        , return (Set_Tup) `apply` parseContents
        ] `adjustErr` ("in <set>, "++)

instance HTypeable Bag where
    toHType x = Defined "bag" [] []
instance XmlContent Bag where
    toContents (Bag a) =
        [CElem (Elem (N "bag") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["bag"]
        ; interior e $ return (Bag) `apply` many parseContents
        } `adjustErr` ("in <bag>, "++)

instance HTypeable Bag_ where
    toHType x = Defined "bag" [] []
instance XmlContent Bag_ where
    toContents (Bag_Locator a) = toContents a
    toContents (Bag_ABool a) = toContents a
    toContents (Bag_AInt a) = toContents a
    toContents (Bag_AFloat a) = toContents a
    toContents (Bag_AString a) = toContents a
    toContents (Bag_AEnum a) = toContents a
    toContents (Bag_Sequ a) = toContents a
    toContents (Bag_Set a) = toContents a
    toContents (Bag_Bag a) = toContents a
    toContents (Bag_Tup a) = toContents a
    parseContents = oneOf
        [ return (Bag_Locator) `apply` parseContents
        , return (Bag_ABool) `apply` parseContents
        , return (Bag_AInt) `apply` parseContents
        , return (Bag_AFloat) `apply` parseContents
        , return (Bag_AString) `apply` parseContents
        , return (Bag_AEnum) `apply` parseContents
        , return (Bag_Sequ) `apply` parseContents
        , return (Bag_Set) `apply` parseContents
        , return (Bag_Bag) `apply` parseContents
        , return (Bag_Tup) `apply` parseContents
        ] `adjustErr` ("in <bag>, "++)

instance HTypeable Tup where
    toHType x = Defined "tup" [] []
instance XmlContent Tup where
    toContents (Tup a) =
        [CElem (Elem (N "tup") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["tup"]
        ; interior e $ return (Tup) `apply` many parseContents
        } `adjustErr` ("in <tup>, "++)

instance HTypeable Tup_ where
    toHType x = Defined "tup" [] []
instance XmlContent Tup_ where
    toContents (Tup_Locator a) = toContents a
    toContents (Tup_ABool a) = toContents a
    toContents (Tup_AInt a) = toContents a
    toContents (Tup_AFloat a) = toContents a
    toContents (Tup_AString a) = toContents a
    toContents (Tup_AEnum a) = toContents a
    toContents (Tup_Sequ a) = toContents a
    toContents (Tup_Set a) = toContents a
    toContents (Tup_Bag a) = toContents a
    toContents (Tup_Tup a) = toContents a
    parseContents = oneOf
        [ return (Tup_Locator) `apply` parseContents
        , return (Tup_ABool) `apply` parseContents
        , return (Tup_AInt) `apply` parseContents
        , return (Tup_AFloat) `apply` parseContents
        , return (Tup_AString) `apply` parseContents
        , return (Tup_AEnum) `apply` parseContents
        , return (Tup_Sequ) `apply` parseContents
        , return (Tup_Set) `apply` parseContents
        , return (Tup_Bag) `apply` parseContents
        , return (Tup_Tup) `apply` parseContents
        ] `adjustErr` ("in <tup>, "++)



{-Done-}

#endif
