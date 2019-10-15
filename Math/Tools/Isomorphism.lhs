>{-# LANGUAGE Trustworthy,FlexibleInstances, MultiParamTypeClasses, RankNTypes, ImpredicativeTypes, Arrows, TypeOperators, LambdaCase, ScopedTypeVariables, TypeFamilies #-}
>module Math.Tools.Isomorphism where
>import Prelude hiding ((.),id)
>import Data.Map (Map)
>import Data.Complex
>import Data.Monoid
>import qualified Data.Map as Map
>import Control.Category
>import Control.Arrow
>import Control.Monad.Fix (fix)
>import Math.Tools.FixedPoint
>import Math.Tools.Integer
>import Math.Tools.Arrow
>import Math.Tools.CoFunctor
>import Math.Tools.Visitor
>import Math.Tools.Adjunction
>import Data.Ratio
>import qualified Data.Binary as Bin
>import Data.ByteString.Lazy (ByteString)
>import qualified Data.ByteString.Lazy as Bytes

>-- | See <http://twanvl.nl/blog/haskell/isomorphism-lenses> I also saw
>-- somewhere (can't remember where) a blog post for an idea for
>-- combinator library using isomorphisms. This is some sort of
>-- implementation of that idea.

>data a :==: b = Iso { isomorphism_epimorphism :: a -> b,
>                     isomorphism_section     :: b -> a }

>type Iso a b = a :==: b
>type Aut a = a :==: a

>-- | <https://en.wikipedia.org/Galois_Connection>
>-- Note that since we don't check the equations for isomorphisms,
>-- this function need not produce identity.
>leftIdempotent :: a :==: b -> Endo a
>leftIdempotent i = Endo $ isomorphism_section i . isomorphism_epimorphism i

>-- | <https://en.wikipedia.org/Galois_Connection>
>-- Note that since we don't check the equations for isomorphisms,
>-- this function need not produce an identity.
>rightIdempotent :: a :==: b -> Endo b 
>rightIdempotent i = Endo $ isomorphism_epimorphism i . isomorphism_section i

>automorphism :: Endo a -> Endo a -> Aut a
>automorphism (Endo f) (Endo g) = f <-> g

>visit_iso :: (ComposableVisitor v) => v :==: a -> v -> a
>visit_iso = visit . embed . isomorphism_epimorphism

>adjoint_iso :: (Adjunction f g) => (f a -> b) :==: (a -> g b)
>adjoint_iso = leftAdjunct <-> rightAdjunct
>

>class (BiArrow arr, Groupoid arr) => Isomorphic arr where
>   iso :: arr a b -> a :==: b

>class IsomorphicFunctor f where
>   data IsoA f a b
>   transformIso :: IsoA f a b -> f a :==: f b

>appIsoF :: (IsomorphicFunctor f) => IsoA f a b -> f a -> f b
>appIsoF = isomorphism_epimorphism . transformIso

>instance (Arrow arr) => ArrowTransformation (:==:) arr where
>   mapA = mapA . runIso

>instance Isomorphic (:==:) where
>   iso = id

>instance Category (:==:) where
>   id = Iso id id 
>   (Iso f f') . (Iso g g') = Iso (f . g) (f' >>> g') 

>instance Groupoid (:==:) where
>   invertA = invertIso

>instance BiArrow (:==:) where
>   (<->) = Iso

>toArrow :: (BiArrow arr) => a :==: b -> arr a b
>toArrow (Iso f g) = f <-> g

>abstract :: (Isomorphic arr) => (a :==: b) :==: arr a b
>abstract = toArrow <-> iso

>runIso :: a :==: b -> a -> b
>runIso (Iso f _) = f

>type FRepresentable f a = forall r. (a -> r) -> f r
>type FCorepresentable f a = forall r. (r -> a) -> f r

>representable :: FRepresentable f a -> f a
>representable f = f id

>corepresentable :: FCorepresentable f a -> f a
>corepresentable f = f id

>inverse_representable :: (Functor f) => f a -> FRepresentable f a
>inverse_representable fa = \g -> fmap g fa

>coinverse_representable :: (CoFunctor f) => f a -> FCorepresentable f a
>coinverse_representable fa = \g -> inverse_image g fa

>(=<) :: a :==: b -> a -> b
>(=<) = runIso

>liftIso :: (Isomorphic arr) => (a :==: b -> c :==: d) -> arr a b -> arr c d
>liftIso f = toArrow . f . iso

>runIsoInverse :: a :==: b -> b -> a
>runIsoInverse = runIso . invertA

>embedIso :: (Groupoid cat) => cat a b -> cat b b -> cat a a
>embedIso f g = f >>> g >>> invertA f

>invertIso :: a :==: b -> b :==: a
>invertIso (Iso f g) = Iso g f

>equalIso :: (Eq a) => (a,a) :==: (Either a (a,a))
>equalIso = (\ (x,y) -> if x==y then Left x else Right (x,y)) 
>            <-> (either (\x -> (x,x)) id)

>inIso :: Rec f :==: f (Rec f)
>inIso = unIn <-> In

>mapIso :: (Functor f) => a :==: b -> f a :==: f b
>mapIso (Iso f g) = fmap f <-> fmap g

>inverseImageIso :: (CoFunctor p) => a :==: b -> p b :==: p a
>inverseImageIso (Iso f g) = Iso (inverse_image f) (inverse_image g)

>bimapIso :: (Functor f, CoFunctor f) => a :==: b -> a :==: c -> f c :==: f b
>bimapIso f g = mapIso f . inverseImageIso g

>-- | constraint: all elements of the map must be present in both maps.
>orderedMapIso :: (Monad m, Ord i, Ord a) => Map i a -> Map a i -> m (i :==: a)
>orderedMapIso n m
>   | Map.keys n == Map.elems m && Map.keys m == Map.elems n = do
>         return $ (n Map.!) <-> (m Map.!)
>   | otherwise = fail "orderedMapIso: keys do not match"

>curryIso :: ((a,b) -> c) :==: (a -> b -> c)
>curryIso = curry <-> uncurry

>intIso :: Aut a -> (Integer -> a) :==: a
>intIso (Iso s p) = (\ f -> f 0) <-> foldInt
>   where foldInt x0 0 = x0
>         foldInt x0 i | i < 0 = p (foldInt x0 (i+1))      
>                      | otherwise = s (foldInt x0 (i-1))

>-- | Cantor pairing function. Algorithm for the inverse is from
>-- <https://secure.wikimedia.org/wikipedia/en/wiki/Pairing_function>

>pairingIso :: (Integer,Integer) :==: Integer
>pairingIso = (\ (x,y) -> let z = x + y in (z*(z+1)) `div` 2 + y)
>             <-> pairing_inverse
>  where pairing_inverse z = (x,y)
>          where w = (square_root_integer (8*z + 1) - 1) `div` 2
>                t = (w * w + w) `div` 2                 
>                y = z - t
>                x = w - y

TODO: negative integers?

>squareIso :: Aut Integer
>squareIso = absIso >>> (sqrIso <||> sqrIso) >>> invertA absIso
>   where sqrIso = (\i -> i*i) <-> square_root_integer



>absIso :: (Num a, Eq a) => a :==: (Either a a)
>absIso = (\i -> (if signum i /= -1 then Left else Right) (abs i))
>           <-> (either id negate)

>plusminusIso :: (Integral a) => Aut (a,a)
>plusminusIso = (\ (x,y) -> (x+y,x-y))
>                 <-> (\(a,b) -> ((a+b)`div` 2, (a-b) `div` 2))

>timesDivideIso :: (Floating a) => Aut (a,a)
>  -- bug: sign is lost. Solution: e^(i*pi) == -1
>  -- bug: y must be non-zero
>timesDivideIso = (\ (x,y) -> (x*y,x/y))
>             <-> (\ (a,b) -> (sqrt(abs a*abs b),
>                              sqrt(abs a / abs b)))


>polarIso :: (RealFloat a) => Complex a :==: (a,a)
>polarIso = polar <-> (uncurry mkPolar)

>ratioIso :: (Integral a) => (a,a) :==: (Ratio a,a)
>ratioIso =  (\ (x,y) -> ((x % y), gcd x y))
>             <-> (\ (r, z) -> (numerator r * z, denominator r * z))

>injectIso :: a :==: b -> (a :==: Either a b, b :==: Either a b)
>injectIso (Iso f f') = (Left <-> either id f', Right <-> either f id)

>leftRightIso :: (a :==: Either a a, a :==: Either a a)
>leftRightIso = injectIso id

>type Bot = forall a. a

>andIso :: (Bool,Bool) :==: (Bool,Ordering)
>andIso = (\(a,b) -> (a && b, compare a b)) <-> \ (x,b) -> case b of
>               LT -> (x,True)
>               EQ -> (x,x)
>               GT -> (True,x)
>
>orIso :: (Bool,Bool) :==: (Bool,Ordering)
>orIso = (\(a,b) -> (a || b, compare a b)) <-> \ (x,b) -> case b of
>           LT -> (False, x)
>           EQ -> (x,x)
>           GT -> (x,False)

>initialIso :: (BiArrow arr, Arrow arr') => arr () (arr' Bot a)
>initialIso =  (\ () -> proc x -> returnA -< undefined)
>          <-> const ()

>terminalIso :: (Isomorphic arr, Arrow arr') => arr () (arr' a ())
>terminalIso =  (\ () -> (proc _ -> returnA -< ()))
>           <-> const ()

>productIso :: (Isomorphic arr, ArrowApply arr')
>           => arr (arr' c (a,b)) (arr' c a, arr' c b)
>productIso = (proc f -> returnA -< (proc c -> do f >>> arr fst -<< c,
>                                    proc c -> do f >>> arr snd -<< c))
>          <-> (proc (f,g) -> returnA -< (proc c -> do 
>                                   r1 <- f -<< c
>                                   r2 <- g -<< c
>                                   returnA -< (r1,r2)))

>coproductIso :: (Isomorphic arr, ArrowChoice arr', ArrowApply arr')
>              => arr (arr' (Either a b) c)
>                     (arr' a c,  arr' b c)
>coproductIso = (proc f -> returnA -< (proc x -> f -<< (Left x),
>                                      proc y -> f -<< (Right y)))
>           <-> (proc (f,g) -> returnA -< (proc z -> case z of
>                        (Left x) ->  f -<< x
>                        (Right y) -> g -<< y))

coexponentialIso :: arr (arr' c (Either a b)) (arr' (arr' c b) a)

>exponentialIso :: (ArrowApply arr') => (arr' (a,b) c) :==: (arr' a (arr' b c))
>exponentialIso =  curr <-> uncurr
>  where curr f = proc a -> returnA -< proc b -> f -< (a,b)
>        uncurr f = proc (a,b) -> do { g <- f -< a ; app -< (g,b) }


>eitherIso :: (Isomorphic arr) => arr a c1 -> arr b c2 
>        -> arr (Either c1 c2) c -> arr (Either a b) c
>eitherIso f g p = toArrow ((iso f <||> iso g) >>> iso p)

>notIso :: (BiArrow arr) => arr Bool Bool
>notIso = not <-> not

>boolIso :: (BiArrow arr) => arr Bool (Either () ())
>boolIso =  (\x -> if x then Left () else Right ())
>          <-> (either (const True) (const False))

>assocIso :: (BiArrow arr) => arr ((a,b),c) (a,(b,c))
>assocIso =   (\ ((x,y),z) -> (x,(y,z)))
>           <-> (\ (x,(y,z)) -> ((x,y),z))

>tripleIso :: (BiArrow arr) => arr (a,b,c) ((a,b),c)
>tripleIso = (\ (x,y,z) -> ((x,y),z))
>            <-> (\ ((x,y),z) -> (x,y,z))

>quadIso :: (BiArrow arr) => arr (a,b,c,d) ((a,b),(c,d))
>quadIso =  (\ (x,y,z,a) -> ((x,y),(z,a)))
>          <->  (\ ((x,y),(z,a)) -> (x,y,z,a))

>pentaIso :: (BiArrow arr) => arr (a,b,c,d,e) ((a,b),c,(d,e))
>pentaIso =  (\ (x,y,z,a,b) -> ((x,y),z,(a,b)))
>           <-> (\ ((x,y),z,(a,b)) -> (x,y,z,a,b))

>zipEither :: (BiArrow arr) => arr (Either x y, Either a b)
>                                  (Either (x,a) (Either (x,b)
>                                    (Either (y,a) (y,b))))
>zipEither = f <-> g
>  where f (Left x,Left a) = Left (x,a)
>        f (Left x,Right b) = Right (Left (x,b))
>        f (Right y, Left a) = Right (Right (Left (y,a)))
>        f (Right y, Right b) = Right (Right (Right (y,b)))
>        g (Left (x,a)) = (Left x, Left a)
>        g (Right (Left (x,b))) = (Left x, Right b)
>        g (Right (Right (Left (y,a)))) = (Right y, Left a)
>        g (Right (Right (Right (y,b)))) = (Right y, Right b)

>distIso :: (BiArrow arr) => arr (Either (a,b) (a,c)) (a,Either b c)
>distIso = f <-> g
>  where  f (Left (a,b)) = (a,Left b)
>         f (Right (a,c)) = (a,Right c)
>         g (a,Left b) = Left (a,b)
>         g (a,Right c) = Right (a,c)

>assocEitherIso :: (BiArrow arr) => arr (Either a (Either b c))
>                                       (Either (Either a b) c)
>assocEitherIso = f <-> g
>   where f (Left x) = Left (Left x)
>         f (Right (Left y)) = Left (Right y)
>         f (Right (Right z)) = Right z
>         g (Left (Left x)) = Left x
>         g (Left (Right y)) = Right (Left y)
>         g (Right z) = Right (Right z)

>terminalSecondIso :: (BiArrow arr) => arr (a,()) a
>terminalSecondIso = (\ (x,_) -> x) <-> (\x -> (x,()))

>terminalThirdIso :: (BiArrow arr) => arr (a,b,()) (a,b)
>terminalThirdIso = (\ (x,y,_) -> (x,y)) <-> (\ (x,y) -> (x,y,()))

>terminalFourthIso :: (BiArrow arr) => arr (a,b,c,()) (a,b,c)
>terminalFourthIso = (\ (x,y,z,_) -> (x,y,z)) <-> (\ (x,y,z) -> (x,y,z,()))

>swapIso :: (BiArrow arr) => arr (a,b) (b,a)
>swapIso = swap <-> swap
>  where swap (x,y) = (y,x)

>swapEitherIso :: (BiArrow arr) => arr (Either a b) (Either b a)
>swapEitherIso = swapEither <-> swapEither

>swapEither :: Either a b -> Either b a
>swapEither (Left x) = Right x
>swapEither (Right x) = Left x

>(<**>) :: a :==: b -> c :==: d -> (a,c) :==: (b,d)
>(<**>) (Iso f f') (Iso g g') = (\ (x,y) -> (f x, g y))
>                           <-> (\ (x,y) -> (f' x, g' y))

>encodeDecode :: (Bin.Binary a) => a :==: Bytes.ByteString
>encodeDecode = Bin.encode <-> Bin.decode

>encoding :: (Bin.Binary a) => Aut Bytes.ByteString -> Aut a
>encoding f = encodeDecode >>> f >>> invertA encodeDecode
>
>decoding :: (Bin.Binary a) => Aut a -> Aut Bytes.ByteString
>decoding f = invertA encodeDecode >>> f >>> encodeDecode

>product3 :: a :==: b -> c :==: d -> e :==: f -> (a,c,e) :==: (b,d,f)
>product3 z z2 z3 = (\ (x,y,z) -> (f x, g y, h z)) <-> (\ (x,y,z) -> (f' x, g' y, h' z))
>   where Iso f f' = iso z
>         Iso g g' = iso z2          
>         Iso h h' = iso z3

>first3 :: a :==: a' -> (a,b,c) :==: (a',b,c)
>first3 f = product3 f id id

>second3 :: b :==: b' -> (a,b,c) :==: (a,b',c)
>second3 f = product3 id f id
> 
>third3 :: c :==: c' -> (a,b,c) :==: (a,b,c')
>third3 f = product3 id id f

>(<||>) :: (Isomorphic arr) => arr a b -> arr c d -> arr (Either a c) (Either b d)
>(<||>) z z2 = (either (Left . f)  (Right . g))
>          <-> (either (Left . f') (Right . g'))
>    where Iso f f' = iso z
>          Iso g g' = iso z2           

>ifIso :: (Isomorphic arr) => arr a b -> arr a c -> arr (a,Bool) (Either b c)
>ifIso g h = toArrow $ (id <**> boolIso)
>                >>> invertA distIso
>                >>> (terminalSecondIso <||> terminalSecondIso)
>                >>> (iso g <||> iso h) 

>branch :: (Isomorphic arr)
>       => arr a b -> arr a c
>       -> arr b d -> arr c d
>       -> arr (a,Bool) (d,Bool)
>branch f g h i = ifIso f g >>> invertA (ifIso (invertA h) (invertA i))


>returnIso :: (BiArrow arr) => a -> arr () a
>returnIso x = (const x) <-> (const ())

>fixIso :: (Isomorphic arr) => arr a a -> arr () (a,a)
>fixIso z = returnIso (fix f, fix f')
>  where Iso f f' = iso z

>foldrIso :: (a,[a]) :==: b -> Bool :==: b -> [a] :==: b
>foldrIso (Iso f f') (Iso b b') = f1 <-> f2
>   where f1 (c:cr) = f (c,cr)
>         f1 [] = b False          
>         f2 zz = if b' zz then let z' = f' (b True) in (fst z':snd z')
>                          else []

>appendIso :: (BiArrow arr) => arr ([a],[a]) ([a],Int) 
>appendIso = f <-> g
>   where f (lst1,lst2) = (lst1++lst2, length lst1)
>         g (lst,i) = splitAt i lst

>listIso :: (BiArrow arr) => arr [a] (Either (a,[a]) ()) 
>listIso = \case { (c:cr) -> Left (c,cr) ; [] -> Right () }
>    <-> (either (\ (a,b) -> (a:b)) (const []))


foldListIso :: (Isomorphic arr) => arr a b -> arr () d -> arr (Either (b,c) d) c -> arr [a] c
foldListIso f g h = let self = listIso >>> ((iso f <**> self) <||> iso g) >>> iso h in toArrow self

>foldListIso :: a :==: b -> () :==: d -> Either (b,c) d :==: c -> [a] :==: c
>foldListIso f g h = let self = listIso >>> ((f <**> self) <||> g) >>> h in self


>-- | This instance of map for list doesn't need fmap:
>mapListIso :: a :==: b -> [a] :==: [b]
>mapListIso f = listIso >>> ((f <**> mapListIso f) <||> id) >>> invertA listIso

>maybeIso :: (BiArrow arr) => arr (Maybe a) (Either a ()) 
>maybeIso = (maybe (Right ()) Left) <-> (either Just (const Nothing))

>data IsomorphismA arr a b = IsoA { epimorphismA :: arr a b,
>                                   sectionA :: arr b a }

>instance (Category arr) => Category (IsomorphismA arr) where
>   id = IsoA id id
>   (IsoA f g) . (IsoA f' g') = IsoA (f . f') (g' . g)

>instance (Arrow arr) => BiArrow (IsomorphismA arr) where
>   f <-> g = IsoA (arr f) (arr g)

>instance (Category arr) => Groupoid (IsomorphismA arr) where
>   invertA (IsoA f g) = IsoA g f
