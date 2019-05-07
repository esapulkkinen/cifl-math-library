>{-# LANGUAGE TypeFamilies, DeriveGeneric, DeriveDataTypeable #-}
>module Math.Tools.BDD where
>import Data.Typeable
>import GHC.Generics
>import Data.Data
>import Math.Tools.Visitor

>-- | Idea for BDD is from
>-- Knuth: The Art of Computer Programming, volume 4, fascicle 1, pg. 70-148.
>-- Every level in BDD tree represents one variable.
>data BDD = BBead { btrue      :: BDD,   -- the BDD corresponding to \v_i -> f(true,v_1,...,v_n)
>                   bfalse     :: BDD }  -- the BDD corresponding to \v_i -> f(false,v_1,...,v_n)
>         | BTrue  -- BDD corresponding to \_ -> True
>         | BFalse -- BDD corresponding to \_ -> False
>  deriving (Data,Typeable,Generic)

>var = BBead BTrue BFalse

>instance Eq BDD where
>   BTrue == BTrue = True
>   BFalse == BFalse = True
>   (BBead f g) == (BBead f' g') = f == f' && g == g'
>   _ == _ = False

>instance Visitor BDD where
>   data Fold BDD a = BDDFold a a (a -> a -> a)
>   visit   (BDDFold t _ _)    BTrue       = t
>   visit   (BDDFold _ f _)    BFalse      = f
>   visit z@(BDDFold _ _ bea) (BBead f g) = bea (visit z f) (visit z g)

>instance Show BDD where
>   show (BBead f g) = "(" ++ show f ++ "," ++ show g ++ ")"
>   show BTrue = "1"
>   show BFalse = "0"

>reduceFold :: Fold BDD BDD 
>reduceFold = BDDFold BTrue BFalse reduceBead
>   where reduceBead f g  | f == g = BBead f f
>                         | otherwise = BBead f g

>reduce :: BDD -> BDD
>reduce = visit reduceFold

>eval :: BDD -> [Bool] -> Bool
>eval BTrue  _ = True
>eval BFalse _ = False
>eval (BBead f g) (c:cr) = if c then eval f cr else eval g cr
>eval _           []     = error "too few arguments to BDD evaluation"

>ifthenelse :: (Bool,[Bool]) -> Either [Bool] [Bool]
>ifthenelse (b,lst) = if b then Left lst else Right lst

>returnBDD :: Bool -> BDD
>returnBDD True = BTrue
>returnBDD False = BFalse

>bead :: BDD -> BDD -> BDD
>bead f g = reduce (BBead f g)

>neg :: BDD -> BDD
>neg BTrue = BFalse
>neg BFalse = BTrue
>neg (BBead f g) = BBead (neg f) (neg g)

>(-&-) :: BDD -> BDD -> BDD
>(BBead f g) -&- (BBead h i) = bead (f -&- h) (g -&- i)
>BTrue -&- x     = x
>x     -&- BTrue = x
>BFalse -&- _    = BFalse
>_ -&- BFalse    = BFalse

>(-|-) :: BDD -> BDD -> BDD
>(BBead f g) -|- (BBead h i) = bead (f -|- h) (g -|- i)
>BFalse -|- x      = x
>x      -|- BFalse = x
>BTrue  -|- _      = BTrue
>_      -|- BTrue  = BTrue

