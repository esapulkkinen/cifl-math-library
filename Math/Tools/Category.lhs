>{-# LANGUAGE GADTs #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Tools.Category where
>import Control.Category

>data CategoryR b c where
>   ReturnR :: CategoryR a a
>   CompR :: CategoryR a b -> CategoryR b c -> CategoryR c d -> CategoryR a d

>reduceCategoryR :: CategoryR b c -> CategoryR b c
>reduceCategoryR ReturnR = ReturnR
>reduceCategoryR (CompR f ReturnR ReturnR) = f
>reduceCategoryR (CompR ReturnR f ReturnR) = f
>reduceCategoryR (CompR ReturnR ReturnR f) = f
>reduceCategoryR (CompR (CompR f g h) i j) = CompR (reduceCategoryR f) (reduceCategoryR (CompR g h i)) (reduceCategoryR j)
>reduceCategoryR (CompR f g (CompR h i j)) = CompR (reduceCategoryR f) (reduceCategoryR (CompR g h i)) (reduceCategoryR j)


class Category a where
      (>>>>) :: a b c -> a c d -> a b d
      idC    :: a b b
      -- laws :  (x >>>> y) >>>> z === x >>>> (y >>>> z)
      --         idC >>>> x === x
      --         x >>>> idC === x

instance Category (->) where
         idC = id
	  x >>>> y = y . x

instance Category (Kleisli IO) where
   idC = returnA
   x >>>> y = x >>> y
