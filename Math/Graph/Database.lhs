>{-# LANGUAGE MultiParamTypeClasses #-}
>module Math.Graph.Database where
>import Control.Applicative
>import Math.Tools.C
>import Math.Tools.Set
>import Math.Graph.Reversible (Graph(..))
>import Data.Map (Map)
>import qualified Data.Map as Map
>import Data.Set (Set)
>import qualified Data.Set as Set

>newtype ColumnName  = NewColumnName { column_index :: Integer }
>   deriving (Eq, Ord)

>newtype RowName     = NewRowName { row_index :: Integer }
> deriving (Eq, Ord)

>data Row a = NewRow { row_contents :: ColumnName -> a }
>data Column a = NewColumn { column_contents :: RowName -> a }
>type TableContents a = (Column :*: Row) a
>data Table a = MakeTable { table_contents :: Graph (ColumnName,RowName) a,
>                           column_by_name :: String -> Maybe ColumnName,
>                           row_by_name    :: String -> Maybe RowName
>                         }

>new_table :: Map String ColumnName
>          -> Map String RowName
>          -> TableContents a
>          -> Table a
>new_table columns rows tabledata = MakeTable g col_by_name row_by_name
>  where
>    g :: Graph (ColumnName,RowName) a
>    g = Graph (joinSet $ outerSet td cols rows) action
>    td i j = tabledata <!> (\c -> column_contents c (columns Map.! i),
>                            \r -> row_contents    r (rows Map.! j))
>    cols = Map.keysSet columns
>    rows = Map.keysSet rows
>    action e act = tabledata <!> (\c -> column_contents c act,
>                                  \r -> row_contents r e)
>    col_by_name cn = Map.lookup cn columns
>    row_by_name r  = Map.lookup r rows

