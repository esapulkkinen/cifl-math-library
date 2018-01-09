>{-# LANGUAGE ExistentialQuantification #-}
>module Math.Tools.Subset where
>import Math.Tools.CoFunctor
>import Data.Set

data Subset b = forall a. Subset { underlying_set    :: Set a, 
                                   subset_embedding  :: a -> b,
                                   subset_membership :: Prop b }

instance CoFunctor Subset where
  inverse_image f (Subset s p) = Subset 
