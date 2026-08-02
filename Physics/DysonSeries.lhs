>import Math.Number.DimensionAnalysis

>data Hamiltonian t a = Hamiltonian {
>   free_part :: a,
>   interacting_part :: t -> a
> } 

>hbar = reducedPlanckConstant

>potential :: Hamiltonian t a -> (t, t) -> a
>potential h (t0, t) = exp exponent * interacting_part h t * exp (negate exponent)
>   where exponent = ((0 :+ (1 / hbar) ) %* free_part h (t - t0))

>data EvolutionOp t where
>  IdOp :: t -> EvolutionOp t
>  FutureOp :: t -> t -> EvolutionOp t
>  CompOp :: EvolutionOp t -> EvolutionOp t -> EvolutionOp t
>  InverseOp :: EvolutionOp t -> EvolutionOp t
>  UnitaryOp :: EvolutionOp t -> EvolutionOp t

>reduceEvolutionOp :: (Eq t) => EvolutionOp t -> EvolutionOp t
>reduceEvolutionOp (FutureOp t t0) | t == t0 = IdOp t0
>                                  | otherwise = FutureOp t t0
>reduceEvoluationOp (Comp z1@(FutureOp t t1) z2@(FutureOp t2 t0))
>   | t1 == t2 = Comp (FutureOp t t0)
>   | otherwise = Comp (reduceEvolutionOp z1) (reduceEvolutionOp z2)
>reduceEvolutionOp (InverseOp (FutureOp t t0)) = FutureOp t0 t
>reduceEvolutionOp (Comp (UnitaryOp (FutureOp t t0)) (FutureOp u u0))
>   | t == u && t0 == u0 = 1
