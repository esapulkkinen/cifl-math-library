>{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, Arrows, TypeFamilies, RankNTypes, Safe #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Tools.OppositeArrow where
>import Prelude hiding (id,(.))
>import Control.Category
>import Control.Arrow
>import Math.Tools.Arrow
>import Math.Tools.CoFunctor

>data OpA arr a b = OpA { runOpA :: arr b a }

>instance (Category c) => Category (OpA c) where
>   id = OpA id
>   (OpA f) . (OpA g) = OpA (g . f)

>instance (ArrowChoice arr) => CoArrow (OpA arr) where
>   coarr f = OpA $ arr f
>   coleft (OpA f) = OpA $ proc z -> case z of
>      (Left a) -> do a' <- f -< a
>                     returnA -< Left a'
>      (Right b) -> returnA -< Right b
>   coright (OpA f) = OpA $ proc z -> case z of
>      (Left a) -> returnA -< Left a
>      (Right b) -> do b' <- f -< b
>                      returnA -< Right b'


>cancelOp :: OpA (OpA cat) a b -> cat a b
>cancelOp (OpA (OpA f)) = f

>inverseImageA :: (Category arr, Arrow arr')
>  => arr a b -> arr' (OpA arr c b) (OpA arr c a)
>inverseImageA f = proc (OpA g) -> returnA -< OpA $ g . f

>instance (Category arr, Arrow arr') => OpArrow (OpA arr a) arr arr' where
>   inverse_imageA = inverseImageA

>instance (Arrow arr) => CoFunctor (OpA arr a) where
>   inverse_image f (OpA x) = OpA (x . arr f)

>data Empty 

>-- | <https://en.wikipedia.org/wiki/Monoidal_category>
>instance MonoidalCategory (OpA (->)) where
>   type Prod (OpA (->)) a b = Either a b
>   type MUnit (OpA (->)) = Empty
>   (OpA f) -*- (OpA g) = OpA $ f +++ g
>   unleftunitor = OpA $ proc z -> case z of { (Right x) -> returnA -< x }
>   unrightunitor = OpA $ proc z -> case z of { (Left x)  -> returnA -< x }
>   leftunitor = OpA $ proc z -> returnA -< (Right z)
>   rightunitor = OpA $ proc z -> returnA -< (Left z)
>   monoidal_assoc = OpA $ proc z -> case z of
>      (Left x) -> returnA -< Left (Left x)
>      (Right (Left y)) -> returnA -< Left (Right y)
>      (Right (Right z)) -> returnA -< Right z
>   monoidal_deassoc = OpA $ proc z -> case z of
>       (Left (Left x)) -> returnA -< Left x
>       (Left (Right y)) -> returnA -< Right (Left y)
>       (Right z) -> returnA -< Right (Right z)
