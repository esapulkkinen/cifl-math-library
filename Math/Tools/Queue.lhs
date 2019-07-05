>{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Arrows, TypeFamilies #-}
>module Math.Tools.Queue where
>import qualified Prelude as P
>import Prelude hiding (unzip,zip,zipWith,reverse,(.),id)
>import qualified Control.Monad as Monad
>import Control.Monad (ap)
>import qualified Data.List as L
>import Control.Category
>import Control.Arrow
>import Control.Applicative
>import Data.IORef
>import Data.Monoid
>import Math.Tools.Arrow
>import Math.Tools.Functor
>import qualified Math.Tools.List as LL
>import Math.Tools.Visitor
>import qualified Text.PrettyPrint as Pretty
>import qualified Math.Tools.PrettyP as PP
>import Math.Tools.PrettyP hiding (empty)
>import Math.Tools.Nondeterministic
>import Math.Matrix.Interface hiding (fromList)

>-- | a queue is really a list with a missing element which is the focus of
>-- the current operations.
>-- This idea comes from zipper pattern. <http://www.haskell.org/haskellwiki/Zipper>

>data Queue a = MakeQueue { queue_newest_first_prefix  :: ![a],
>                           queue_oldest_first_postfix :: ![a] }

>-- | This version of queue is not a comonad, since empty queue is possible.
>instance (Num a) => VectorSpace (Queue a) where
>  type Scalar (Queue a) = a
>  vzero = MakeQueue [] []
>  vnegate (MakeQueue lst lst') = MakeQueue (map negate lst) (map negate lst')
>  v %* (MakeQueue lst lst') = MakeQueue (map (v*) lst) (map (v*) lst')
>  q %+ q' = q `append` q'

>newQueueM :: IO (IORef (Queue a))
>newQueueM = newIORef empty

>enqueueM :: IORef (Queue a) -> a -> IO ()
>enqueueM ref x = modifyIORef ref (enqueue x)

>dequeueM :: IORef (Queue a) -> IO a
>dequeueM ref = do q <- readIORef ref
>                  (x,q') <- dequeue q                   
>                  writeIORef ref q'
>                  return x

>split_list :: [a] -> Int -> (a,Queue a)
>split_list lst i = (lst !! i, MakeQueue (take i lst) (L.reverse $ drop (i+1) lst))

>join_list :: a -> Queue a -> [a]
>join_list x (MakeQueue pre post) = pre ++ (x : L.reverse post)

>instance Functor Queue where
>   fmap f (MakeQueue x y) = MakeQueue (map f x)  (map f y)

>instance Monad Queue where
>   return x = MakeQueue [x] []
>   q >>= f = join' $ fmap f q
>   fail _ = MakeQueue [] []

>instance Nondeterministic Queue where
>   guess = fromList

>instance Applicative Queue where
>   pure = return
>   (<*>) = ap

>instance Alternative Queue where
>   empty = MakeQueue [] []
>   (<|>) = append

>isEmpty :: Queue a -> Bool
>isEmpty (MakeQueue [] []) = True
>isEmpty _ = False

>instance Visitor (Queue a) where
>   data Fold (Queue a) b = QueueFold b (a -> b -> b)
>   visit z@(QueueFold x f) q = maybe x p (dequeue_back q)
>       where p ~(b,a) = f a (visit z b)

>instance (PpShow a) => Show (Queue a) where
>   show = pPrint

>instance (PpShow a) => PpShow (Queue a) where
>   pp (MakeQueue x y) = pp "queue{" 
>        <> Pretty.cat (Pretty.punctuate (pp ',') $ map pp x)
>        <> (if null x then Pretty.empty else pp ',')
>        <> pp "!"
>        <> (if null y then Pretty.empty else pp ',')
>        <> Pretty.cat (Pretty.punctuate (pp ',') $ map pp (L.reverse y))
>        <> pp '}'                 

cat (punctuate (pp ',') $ map pp (x ++ L.reverse y)) <> pp '}'

>instance (ArrowChoice ar) => FunctorArrow Queue ar where
>   amap f = proc (MakeQueue x y) -> do
>               x' <- amap f -< x
>               y' <- amap f -< y
>               returnA -< MakeQueue x' y'

>-- | join works in round-robin fashion. Note if all inner queues are empty,
>-- this goes to infinite loop trying to find elements from empty queues.
>join :: Queue (Queue a) -> Queue a
>join = visit (QueueFold empty interleave)

>-- | Alternative implementation of join, works in reverse compared to join.

>join' :: Queue (Queue a) -> Queue a
>join' q = maybe empty f (dequeue q)
>   where f ~(q',r) = maybe (join' r) (g r) (dequeue q')
>         g r ~(v,q'') = enqueue v (join' (enqueue q'' r))

>fold :: (Arrow arr) => (a -> arr b b) -> Queue a -> arr b b
>fold f (MakeQueue x y) = foldr (\xx r -> f xx >>> r) returnA (y ++ L.reverse x)

>instance InterleaveFunctor Queue where
>  interleave = interleave_queue

>interleave_queue :: Queue a -> Queue a -> Queue a
>interleave_queue (MakeQueue a b) (MakeQueue c d) = MakeQueue (interleave a c)
>                                                       (interleave b d)

empty comes from Alternative

>singleton :: a -> Queue a
>singleton x = enqueue x empty

>enqueue :: a -> Queue a -> Queue a
>enqueue x (MakeQueue a b) = MakeQueue (x:a) b 

>enqueue_back :: a -> Queue a -> Queue a
>enqueue_back x (MakeQueue a b) = MakeQueue a (x:b) 

>dequeue :: (Monad m) => Queue a -> m (a,Queue a)
>dequeue (MakeQueue [] []) = fail "cannot dequeue from an empty queue"
>dequeue (MakeQueue x (x':r')) = return (x',MakeQueue x r')
>dequeue (MakeQueue x []) = dequeue (MakeQueue [] (L.reverse x))

>dequeue_back :: (Monad m) => Queue a -> m (Queue a,a)
>dequeue_back (MakeQueue [] []) = fail "cannot dequeue_back an empty queue"
>dequeue_back (MakeQueue (x:xr) r) = return (MakeQueue xr r,x)
>dequeue_back (MakeQueue [] r) = dequeue_back $ MakeQueue (L.reverse r) []

>dequeue_rotate :: (Monad m) => Queue a -> m (a,Queue a)
>dequeue_rotate q = do { (x,q') <- dequeue q ; return (x,enqueue x q') }

>dequeue_rotate_back :: (Monad m) => Queue a -> m (Queue a,a)
>dequeue_rotate_back q = do (q',x) <- dequeue_back q
>                           return (enqueue_back x q',x)

>rotate :: Queue a -> Queue a
>rotate q = maybe q (uncurry enqueue) (dequeue q)

>rotate_back :: Queue a -> Queue a
>rotate_back q = maybe q (uncurry enqueue_back . swap) (dequeue_back q)
>   where swap (x,y) = (y,x)

>reduce :: Queue a -> Queue a
>reduce (MakeQueue x y) = MakeQueue [] (y ++ L.reverse x)

>unreduce :: Queue a -> Queue a
>unreduce (MakeQueue x y) = MakeQueue (x ++ L.reverse y) []

>fromList :: [a] -> Queue a
>fromList lst = MakeQueue lst []

>toList :: Queue a -> [a]
>toList (MakeQueue x y) = x ++ L.reverse y

>unzip :: Queue (a,b) -> (Queue a, Queue b)
>unzip (MakeQueue x y) = let (x',x'') = P.unzip x
>                            (y',y'') = P.unzip y
>                         in (MakeQueue x' y', MakeQueue x'' y'')

>zip :: Queue a -> Queue b -> Queue (a,b)
>zip q1 q2 = let MakeQueue _ q1' = reduce q1
>                MakeQueue _ q2' = reduce q2
>             in MakeQueue [] (P.zip q1' q2')

>partitionEither :: Queue (Either a b) -> (Queue a, Queue b)
>partitionEither (MakeQueue x y) = let (x',x'') = L.partition isLeft x
>                                      (y',y'') = L.partition isLeft y
>                                      isLeft (Left _) = True
>                                      isLeft _        = False
>                                      fromLeft (Left xx) = xx
>                                      fromLeft _ = undefined
>                                      fromRight (Right yy) = yy
>                                      fromRight _ = undefined
>                                    in (MakeQueue (map fromLeft x')
>                                                  (map fromLeft y'), 
>                                        MakeQueue (map fromRight x'')
>                                                  (map fromRight y''))

>append :: Queue a -> Queue a -> Queue a
>append (MakeQueue x x') (MakeQueue y y') = MakeQueue (x ++ L.reverse x')
>                                                     (L.reverse y ++ y')

>dequeue_current :: (Monad m) => Queue a -> m (a,Queue a)
>dequeue_current (MakeQueue x (c:cr)) = return (c,MakeQueue x cr)
>dequeue_current (MakeQueue x []) = fail "At end of queue"

>replace_current :: a -> Queue a -> Queue a
>replace_current x (MakeQueue r (_:cr)) = MakeQueue r (x:cr)
>replace_current x (MakeQueue r []) = MakeQueue r [x]

>forward :: a -> Queue a -> Queue a
>forward _ (MakeQueue r (c:cr)) = MakeQueue (c:r) cr
>forward x (MakeQueue r []) = MakeQueue (x:r) []

>backward :: a -> Queue a -> Queue a
>backward _ (MakeQueue (c:cr) r) = MakeQueue cr (c:r)
>backward x (MakeQueue [] r) = MakeQueue [] (x:r)


>reverse :: Queue a -> Queue a
>reverse (MakeQueue x y) = MakeQueue (L.reverse y) (L.reverse x)

>sequence :: (Monad m) => Queue (m a) -> m (Queue a)
>sequence (MakeQueue f g) = do fr <- Monad.sequence f
>                              gr <- Monad.sequence g                               
>                              return (MakeQueue fr gr)

>mapM :: (Monad m) => (a -> m b) -> Queue a -> m (Queue b)
>mapM f (MakeQueue a b) = do a' <- Monad.mapM f a
>                            b' <- Monad.mapM f b                             
>                            return (MakeQueue a' b')

