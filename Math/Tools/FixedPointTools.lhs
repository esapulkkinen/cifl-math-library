>{-# LANGUAGE ExistentialQuantification, TypeOperators #-}
>module Math.Tools.FixedPointTools where
>import Control.Monad.Fix
>import Math.Tools.FixedPoint
>import Math.Tools.Orthogonal
>import Math.Tools.Adjunction
>import Math.Tools.Pair
>import Math.Matrix.Interface
>import Math.Matrix.Matrix

>make_sink :: (Functor f) => (a -> f b) -> (f c -> b -> c) -> (b -> c) -> Rec ((->) a)
>make_sink x y = unfold (\f -> fold_one x f y)

>use_sink :: (Functor f) => (a -> f b) -> (f (a -> c) -> c) -> Rec ((->) b) -> a -> c
>use_sink x y = fold (\f -> fold_one x f y)

>fold_one :: (Functor f) => (a -> f b) -> (b -> c) -> (f c -> d) -> a -> d
>fold_one a f b = b . fmap f . a

>map_function :: (Functor f, Functor g) => (a -> f b, b -> g c) -> (g x -> y, f y -> z)
>					-> (c -> x) -> a -> z
>map_function (a1,a2) (b2,b1) f = fold_one a1 (fold_one a2 f b2) b1

>map_rec :: (Functor f, Functor g) => (g a -> a) -> (a -> f a) -> Rec g -> Rec f
>map_rec destruct construct = unfold construct . fold destruct

>create_simple :: (Functor f) => (a -> f a) -> f a -> Rec f
>create_simple = unfold . fmap

>destroy_simple :: (Functor f) => (f b -> b) -> Rec f -> f b
>destroy_simple = fold . fmap

fold_pair :: (Functor f) => (f (b,c) -> b) -> (f (b,c) -> c) -> Rec f -> (b,c)
fold_pair f = fold . pair f


newtype Object f a = Object (Rec (f a))

>infinite_dimensional_outer :: (Functor f, Functor g) => Rec f -> Rec g -> Rec (f :*: g)
>infinite_dimensional_outer = fix inf_outer_loop
>    where inf_outer_loop f (In x) (In y) = In $ matrix f x y


>fold2 :: (Functor f, Functor g) => (f a -> g a -> a) -> Rec f -> Rec g -> a
>fold2 f x y = x `foldBind` \a ->
>	       y `foldBind` f a -- fold (\a -> fold (f a) y) x

>foldouter2 :: (Functor f, Functor g) =>
>	   (f (g c) -> f (g c) -> c) -> Rec f -> Rec g -> f (g c)
>foldouter2 = fold2 . outer

>outerfold2 :: (Functor f, Functor g) =>
>	    (Rec f -> Rec g -> c) -> (a -> f a) -> (b -> g b) -> a -> b -> c
>outerfold2 f x y = outer f (unfold x) (unfold y)

>foldBind :: (Functor f) => Rec f -> (f a -> a) -> a
>foldBind = flip fold

>unfoldBind :: (Functor f) => a -> (a -> f a) -> Rec f
>unfoldBind = flip unfold

>toRec :: (Functor f, InitialAlgebra f a) => a -> Rec f
>toRec = fold_gen In

>fromRec :: (FinalCoalgebra f a) => Rec f -> a
>fromRec = unfold_gen unIn


>invert_rec :: (Adjunction path space) => Rec path -> Rec space
>invert_rec = fold (rightAdjunct unIn)

>inverse_compose :: (Adjunction path space) => (a -> path a) -> (space b -> b) -> a -> b
>inverse_compose f g = fold g . invert_rec . unfold f

>fix_rec :: (Functor f) => (Rec f -> Rec f) -> Rec f
>fix_rec f = In . fmap f . unIn $ fix_rec f

>fixouter :: (Functor f) => (a -> f a) -> f a -> f (Rec f)
>fixouter f x = x `mapf` \a -> In (fixouter f (f a))

>allouter :: (a -> a -> a) -> (a -> a) -> a -> a -> Rec ((->) a)
>allouter f x = x `mapf` \a -> f a `mapf` (In . allouter f (f a))

>fixouter_lst f x = map (\a -> In (fixouter_lst f (f a))) x

>fold_outer :: (Adjunction path space) => (bot -> path bot -> bot)
>	    -> Rec path -> space bot
>fold_outer = fold . full_outer counit unit


>infinite_stack :: (a -> a -> a) -> Rec ((->) a)
>infinite_stack = In . unfold

>infinite_stack' :: ((c,b) -> c) -> c -> Rec ((->) b)
>infinite_stack' = unfold . curry

>count_type :: (Adjunction path space) => (path bot -> bot) -> bot -> Rec space
>count_type = unfold . leftAdjunct

>fold_path :: (Adjunction path space) => (a -> space a) -> Rec path -> a
>fold_path = fold . rightAdjunct

>create_space :: (Adjunction path space, Functor f) => 
>		 (path (f (space bot)) -> bot) -> Rec f -> space bot
>create_space = fold . leftAdjunct

>create_fixspace :: (Adjunction path space) => Rec path -> space (Rec path)
>create_fixspace = leftAdjunct In


>data X c = X (c -> c)

>self_apply :: (Rec X -> Rec X) -> Rec X
>self_apply x = x (In (X x))

>unX :: Rec X -> Rec X -> Rec X
>unX (In (X a)) = a


>mfold_list_maybe :: (Monad m) => (Maybe (x,y) -> m y) -> [x] -> m y 
>mfold_list_maybe f (x:y) = mfold_list_maybe f y >>= \r -> f (Just (x,r))
>mfold_list_maybe f []    = f Nothing

>mfold_list :: (Monad m) => (x -> y -> m y) -> m y -> [x] -> m y
>mfold_list f z (x:y) = mfold_list f z y >>= \r -> f x r
>mfold_list _ z []    = z

>mmap_list :: (Monad m) => (x -> m y) -> [x] -> m [y]
>mmap_list f (x:xs) = f x >>= \v -> mmap_list f xs >>= \vr -> return (v:vr)

-- mmap_list id == sequence

instance Functor (Either a) where -- already in Control.Monad.Error
	  fmap f (Left x) = Left x
	  fmap f (Right y) = Right (f y)

>build_either :: (a -> c) -> (Either c d -> d) -> Rec (Either a) -> Either c d
>build_either x y = fold (cases x y)

>fold_twice :: (Functor f, Functor g) => (g (f a -> a) -> f a -> a) -> Rec g -> Rec f -> a
>fold_twice x y = fold (fold x y)

>data StrictList a = forall b. StrictList (b -> Maybe (a,b)) b

>outer_product' :: (Functor f, Functor g) => f a -> g b -> f (g (b,a))
>outer_product' = outer unit
