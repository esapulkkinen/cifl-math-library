>module Math.Tools.Nondeterministic where
>import System.Random

The problem with Nondeterministic class is that going through the
whole list will cause exponential behaviour _in the length_ of the
representation of the type.

>class (Monad m) => Nondeterministic m where
>   guess :: [a] -> m a

If the original set of elements is stored to the monad, then the
following is possible.

>class (Nondeterministic m) => Classified m where
>   solve :: m a -> [a]

>instance Nondeterministic [] where { guess = id }
>instance Classified [] where { solve = id }

>plus_nondet :: (Nondeterministic m) => m a -> m a -> m a
>plus_nondet a b = a >>= \v -> b >>= \w -> guess [v,w]

>zero_nondet :: (Nondeterministic m) => m a
>zero_nondet = guess []

The invert method will choose an element of 'a' from those elements
for which the given function will produce the given value of b.

Thus: invert f x \in {a | f a == x}. The value chosen is arbitrary.

Note this is an expression of the axiom of choice.  The function 'f'
must be surjective, because otherwise the set {a | f a == x} does not
have exactly one element.

Minimal sufficient implementation: invert. Other methods are for efficiency.

>class (Monad m) => ChoiceMonad m where
>   invert :: (Random a, Eq b) => (a -> m b) -> b -> m a
>   random_element :: (Random a) => m a
>   random_element = invert (const (return ())) ()


>invert_propertyM ::  (ChoiceMonad m, Random a) => (a -> m Bool) -> m a
>invert_propertyM f = invert f True

>invert_property :: (ChoiceMonad m, Random a) => (a -> Bool) -> m a
>invert_property f = invert_propertyM (return . f)
