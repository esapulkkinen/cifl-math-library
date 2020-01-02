>{-# LANGUAGE Trustworthy, OverloadedLists, TypeFamilies #-}
>module Math.Number.Unsafe where

>import GHC.Exts (IsList(..))
>import Math.Number.Stream

>instance IsList (Stream a) where
>  type Item (Stream a) = a
>  fromList = Math.Number.Stream.cycle
>  toList = Math.Number.Stream.toList
