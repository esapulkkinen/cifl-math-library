>module Math.Tools.Multimap where
>import Control.Exception
>import Data.Typeable
>import Data.Map (Map)
>import qualified Data.List as List
>import qualified Data.Map as Map
>import Math.Tools.Exception
>
>data Multimap k v = Multimap { multimap_rep :: Map k [v] }
>
>data MultimapException = KeyNotFoundFromMultimap | EmptyValueFoundFromMultimap | AmbiguousValueInMultimap 
>  deriving (Show, Typeable)

>instance Exception MultimapException

>empty :: Multimap k v
>empty = Multimap $ Map.empty

>null :: Multimap k v -> Bool
>null (Multimap m) = Map.null m

>singleton :: k -> v -> Multimap k v
>singleton k v = Multimap $ Map.singleton k [v]

>(!) :: (Ord k) => Multimap k v -> k -> [v]
>(Multimap m) ! k = m Map.! k

>size :: Multimap k a -> Int
>size = sum . map length . Map.elems . multimap_rep
>
>member :: (Ord k) => k -> Multimap k a -> Bool
>member k (Multimap m) = k `Map.member` m

>notMember :: (Ord k) => k -> Multimap k a -> Bool
>notMember k (Multimap m) = k `Map.notMember` m

>insert :: (Ord k) => k -> v -> Multimap k v -> Multimap k v
>insert k v (Multimap m)
>   | Just v' <- Map.lookup k m = Multimap $ Map.insert k (v : v') m
>   | otherwise = Multimap $ Map.insert k [v] m

>delete :: (Ord k, Eq v) => k -> v -> Multimap k v -> Multimap k v
>delete k v (Multimap m) = Multimap $ Map.adjust (List.delete v) k m
>
>union :: (Ord k) => Multimap k v -> Multimap k v -> Multimap k v
>union (Multimap m) (Multimap n) = Multimap $ Map.unionWith (++) m n
>
>intersection :: (Ord k, Eq v) => Multimap k v -> Multimap k v -> Multimap k v
>intersection (Multimap m) (Multimap n) = Multimap $ Map.intersectionWith List.intersect m n

>lookup :: (ExceptionalMonad m, Ord k) => k -> Multimap k v -> m [v]
>lookup k (Multimap m) | Just x <- Map.lookup k m = return x
>                      | otherwise = throwM $ KeyNotFoundFromMultimap

>lookupUnique :: (ExceptionalMonad m, Ord k, Show k, Show v) => k -> Multimap k v -> m v
>lookupUnique key (Multimap m)
>           | Just x <- Map.lookup key m = assertUnique x
>           | otherwise = throwM $ KeyNotFoundFromMultimap
>
>assertUnique :: (ExceptionalMonad m) => [x] -> m x
>assertUnique [] = throwM $ EmptyValueFoundFromMultimap
>assertUnique [x] = return x
>assertUnique lst = throwM $ AmbiguousValueInMultimap 
