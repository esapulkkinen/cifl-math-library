>module Math.Graph.Set where
>import Data.Set
>import Control.Category
>import Control.Arrow
>import Math.Tools.Arrow
>import Prelude hiding (id, (.))
>
>data SetHom arr a b = HomSet { set_morphism :: arr (Set a) (Set b) }

>instance (Category arr) => Category (SetHom arr) where
>   id = HomSet id
>   (HomSet f) . (HomSet g) = HomSet (f . g)

>instance (Arrow a) => Arrow (SetHom a) where
>   arr f = HomSet $ amap f
>   first (HomSet f) = HomSet $ amap (first f)
>   second (HomSet f) = HomSet $ amap (second f)
>   (HomSet f) *** (HomSet g) = HomSet $ f *** g
>   (HomSet f) &&& (HomSet g) = HomSet $ f &&& g

>element :: a -> SetHom arr () a
>element x = HomSet $ arr (const x)
