>module Math.Tools.Projection where
>import Prelude hiding (id,(.))
>import Control.Category
>import Control.Arrow

>data Projection arr a b = Projection { getP :: arr a b,
>                                       setP :: arr b b -> arr a a }

>instance (Category arr) => Category (Projection arr) where
>  id = Projection id id
>  (Projection g s) . (Projection g' s') = Projection (g . g') (s' . s)

>fstP :: (Arrow arr) => Projection arr (a,b) a
>fstP = Projection (arr fst) first

>sndP :: (Arrow arr) => Projection arr (a,b) b
>sndP = Projection (arr snd) second

