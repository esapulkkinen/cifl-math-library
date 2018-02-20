>{-# LANGUAGE KindSignatures, DataKinds, GADTs, ConstraintKinds, RankNTypes, AllowAmbiguousTypes #-}
>module Math.Matrix.FiniteQueue where
>import qualified Math.Matrix.FiniteVector as Vec
>import Math.Matrix.FiniteVector

>data Que :: Nat -> Nat -> * -> * where
>  Que :: Vec n a -> Vec m a -> Que n m a

>data EncQue a = forall n m. EncQue (Que n m a)

>toList :: Que n m a -> [a]
>toList (Que x y) = Vec.toList x ++ Vec.toList y

>toRightEdge :: EncQue a -> EncQue a
>toRightEdge (EncQue (Que (Cons n m) p)) = toRightEdge $ EncQue (Que m (Cons n p))
>toRightEdge (EncQue (Que Empty p)) = EncQue (Que Empty p)

>toLeftEdge :: EncQue a -> EncQue a
>toLeftEdge (EncQue (Que p (Cons n m))) = toLeftEdge $ EncQue (Que (Cons n p) m)
>toLeftEdge (EncQue (Que p Empty)) = EncQue (Que p Empty)

>toRight :: Que ('Succ n) m a -> Que n ('Succ m) a
>toRight (Que (Cons n m) p) = Que m (Cons n p)

>toLeft :: Que n ('Succ m) a -> Que ('Succ n) m a
>toLeft (Que n (Cons m p)) = Que (Cons m n) p

