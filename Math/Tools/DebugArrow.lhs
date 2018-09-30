>{-# LANGUAGE Arrows, MultiParamTypeClasses, GADTs, FlexibleInstances #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Tools.DebugArrow where
>import Prelude hiding ((.),id)
>import Control.Category
>import Control.Arrow
>import Data.List

>data Controls = Run
>              | StepToTag String Controls
>              | StepNextTag Controls
>              | StepNextPrim Integer Controls

>class Breakpoint arr x cntr where
>   breakpoint_action :: arr (String,cntr,x) (cntr,x)

>data DebugA cntr arr i o where
>   DebugAAtomic ::  [String] -> arr (cntr,i) (cntr,o) -> DebugA cntr arr i o
>   DebugABreakpoint :: (Breakpoint arr x cntr) => 
>                       String
>                    -> DebugA cntr arr i x
>                    -> DebugA cntr arr x o
>                    -> DebugA cntr arr i o

>runDebugA :: (Arrow arr) => DebugA cntr arr i o -> arr (cntr,i) (cntr,o)
>runDebugA (DebugAAtomic _ f) = f
>runDebugA (DebugABreakpoint _ f g) = runDebugA f >>> runDebugA g

>tags :: DebugA cntr arr i o -> [String]
>tags (DebugAAtomic tags _) = tags
>tags (DebugABreakpoint t f g) = [t] `union` tags f `union` tags g

>step :: (ArrowChoice arr) => arr (Controls,x) (Controls,x)
>step = proc (c,x) -> case c of
>         (StepNextPrim 0 c') -> returnA -< (c',x)
>         (StepNextPrim i c') -> returnA -< (StepNextPrim (i-1) c',x)
>         z -> returnA -< (z,x)

>instance (Arrow arr) => Category (DebugA Controls arr) where
>   id = DebugAAtomic [] id
>   h . (DebugABreakpoint tag f g) = (DebugAAtomic t $ proc (ctr,i) -> do
>                                       (ctr',j) <- runDebugA f -< (ctr,i)
>                                       (ctr'',j') <- breakpoint_action -< (tag,ctr',j)
>                                       runDebugA g -< (ctr'',j')) >>> h
>              where t = [tag] `union` tags f `union` tags g
>   (DebugAAtomic t2 g) . (DebugAAtomic t1 f)  = DebugAAtomic (t1 `union` t2) (f >>> g)
>   (DebugABreakpoint tag g h) . (DebugAAtomic t1 f) = DebugAAtomic (t1 `union` [tag]) $ proc (ctr,i) -> do
>        (ctr',j) <- f -< (ctr,i)
>        (ctr'',k) <- runDebugA g -< (ctr',j)
>        (ctr''',k') <- breakpoint_action -< (tag,ctr'',k)
>        runDebugA h -< (ctr''',k')

>instance (ArrowChoice arr) => Arrow (DebugA Controls arr) where
>   arr f = DebugAAtomic [] (step >>> second (arr f))
>   first (DebugAAtomic t1 f) = DebugAAtomic t1 (proc (c,(i,x)) -> do
>                                      (c',j) <- f -< (c,i)
>                                      returnA -< (c',(j,x)))
>   first (DebugABreakpoint tag f g) = DebugAAtomic [tag] $ proc (c,(i,x)) -> do
>               (c',j) <- runDebugA f -< (c,i)
>               (c'',j') <- breakpoint_action -< (tag,c',j)
>               (ce,j'') <- runDebugA g -< (c'',j')
>               returnA -< (ce,(j'',x))
