>{-# LANGUAGE RankNTypes, TypeOperators, MultiParamTypeClasses, ExistentialQuantification #-}
>module Math.Tools.Arrows where
>import Math.Tools.Adjunction
>import Math.Tools.NaturalTransformation

>class CategoryA a where
>      idA :: a b b
>      (>>>) :: a c d -> a b c -> a b d

>class (CategoryA a) => NaturalA a z where
>      componentA :: f :~> g -> a (f z) (g z)

>class (CategoryA a) => FunctorA a f where
>      mapA :: a b c -> a (f b) (f c)

>class (CategoryA a) => ProductA a where
>      productA :: a b c -> a b d -> a b (c,d)
>      fstsndA   :: (a (b,c) b, a (b,c) c)
>      crossA :: a b c -> a d e -> a (b,d) (c,e)

>class (CategoryA a) => CartesianA a where
>      lambdaA :: a (ctx,b) c -> a ctx (b -> c)
>      applyA  :: a (b -> c, b) c

>class (CategoryA a) => ChoiceA a where
>      plusA :: a b c -> a d e -> a (Either b d) (Either c e)
>      whileLeftA  :: a (Either a' c) (Either b c) -> a a' b
>      whileRightA :: a (Either c a') (Either c b) -> a a' b

>class (CategoryA a) => AlternativesA a where
>      joinAltsA :: a (Either b b) b
>      inlA :: a b (Either b c)
>      inrA :: a c (Either b c)

Adaptation of ideas from: Roy L. Crole: Categories for Types.

class (CategoryA a) => LimitA a where
      limA :: (Const c) :~> f -> (Const (Lim f)) :~> f -> a c (Lim f)
      colimA :: f :~> (Const c) -> f :~> (Const (Colim f)) -> a (Colim f) c

data Const c a = Const c
data Lim f dom = Lim dom (forall a. f a)
data Colim f dom = forall a. Colim dom (f a)

>class DataStoreA a where
>      delayA :: b -> a b b
