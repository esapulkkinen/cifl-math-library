>{-# LANGUAGE Safe,FlexibleInstances, ScopedTypeVariables #-}
>{-# LANGUAGE OverloadedStrings #-}
>-- | The module contains only Show and PpShow instances for graphs.
>module Math.Graph.Show where
>import Text.PrettyPrint (Doc, empty, (<+>), punctuate)
>import qualified Data.Text as T
>import Data.Monoid hiding ((<>))
>import qualified Data.Set as Set
>import qualified Math.Tools.PrettyP as PrettyP
>import Math.Tools.PrettyP
>import Data.Monoid
>import Data.Set (Set)
>import Data.Binary
>import Control.Monad.Trans.Class
>import Math.Graph.Interface
>import Math.Graph.TransformationMonoid
>import Math.Graph.GraphMonoid

>base :: Doc
>base = pp ("[]" :: T.Text)

>eprint :: (PpShow e, PpShow x, PpShow y) => (e,x,y) -> Doc
>eprint (e,x,y) = pp e <+> pp '=' <+> pp x <+> pp ("->" :: T.Text) <+> pp y

>bidirectionalEprint :: (Eq e, PpShow e, PpShow x, PpShow y) => ((e,e),(x,y)) -> Doc
>bidirectionalEprint ((e,re),(x,y)) = pp e
>      <+> (if e /= re then pp ("/" :: T.Text) <+> pp re else empty)
>      <+> pp '=' <+> pp x <+> pp ("<->" :: T.Text) <+> pp y

>veprint :: (PpShow a, PpShow b) => ([a], [b]) -> Doc
>veprint (v,e) = pp ("(" :: T.Text)
>             <> (PrettyP.ppList v <+> pp ';' <+> PrettyP.ppList e)
>             <> pp ')'

