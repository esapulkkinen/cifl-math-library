>{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleInstances #-}
>module Physics.Physics where
>import Control.Applicative
>import Data.Maybe
>import Data.Char
>import Data.Complex
>import Data.Ratio
>import Math.Tools.Isomorphism
>import Math.Tools.Complex
>import Math.Tools.PrettyP
>import Math.Matrix.Interface
>import Math.Number.DimensionalAnalysis
>import Math.Matrix.Covector
>import Math.Number.Stream
>import qualified Math.Number.Stream as Stream
>import Math.Matrix.Vector3
>import Math.Matrix.Vector4
>import Math.Number.Real
>import Math.Number.Units
>import Text.PrettyPrint hiding ((<>))
>type FourVector a = Vector4 (Complex (Quantity a))

>spacetime :: Vector3 Length -> Time -> Vector4 (Complex Length)
>spacetime (Vector3 x y z) t = Vector4 (x :+ m0) (y :+ m0) (z :+ m0) (m0 :+ ct)
>  where m0 = Meters 0
>        c = fromJust (speed_of_light `asUnit` MetersPerSecond)
>        ct = fromJust ((c *% t) `convert` meter >>= (return . fromAmount))

>-- | <http://en.wikipedia.org/wiki/Klein%E2%80%93Gordon_equation>

dAlembertian :: Dual (Vector4 (Complex R)) -> Dual (Vector4 (Complex R))

>dAlembertian :: (Show a,RealFloat a, Closed a, Infinitesimal a) =>
>   Dual (FourVector a) -> Dual (FourVector a)
>dAlembertian f = (1 / (c * c)) %* 
>         (derivate4t . derivate4t) f
>       - (derivate4x . derivate4x) f
>       - (derivate4y . derivate4y) f
>       - (derivate4z . derivate4z) f
>  where c = speed_of_light :+ 0

>planewave omega k (Vector4 x y z t) = exp (negate (0 :+ 1) * omega * t + (0 :+ 1)*k*x)




einstein_field_equation r g lam t = r - (r %*% g) / 2 + lam %* g == 8*pi*


time_dependent_schrodinger hamiltonian theta r t = imaginary_unit %* quantity planck_constant * theta r t - hamiltonian (theta r t)



kleingordon :: Complex R
            -> Dual (Vector4 (Complex R)) -> Dual (Vector4 (Complex R))

>kleingordon :: (Closed a, Infinitesimal a, RealFloat a, Show a)
>   => Complex (Quantity a) -> Dual (FourVector a) -> Dual (FourVector a)
>kleingordon m f = dAlembertian f %+ dconst (mu * mu)
>   where mu = m * (c / h)
>         c = speed_of_light :+ 0          
>         h = reduced_planck_constant :+ 0

>klein_gordon :: (Floating a, Show a, Closed a, Infinitesimal a) =>
>   Quantity a -> Dual (Vector4 (Quantity a)) -> Dual (Vector4 (Quantity a))
>klein_gordon mass f = Covector allterms
>   where allterms v = firstterm v - secondterm v + thirdterm v
>         h2 = planck * planck
>         der2 = derivate4t . derivate4t          
>         mch = (mass * c) / planck
>         c = speed_of_light
>         planck = reduced_planck_constant 
>         firstterm v = (der2 f `bracket` v) / (c*c)
>         secondterm v = laplace4 f `bracket` v
>         thirdterm v = mch * mch * (f `bracket` v)

>-- | <http://en.wikipedia.org/wiki/Periodic_Table>

>periodic_table = [(0,"Neutron"),(1,"H"),(2,"He"),(3,"Li"),(4,"Be"),(5,"B"),(6,"C"),
>                        (7,"N"),(8,"O"),(9,"F"), (10,"Ne"), (11,"Na"),(12,"Mg"),
>                        (13,"Al"),(14,"Si"),(15,"P"),(16,"S"),(17,"Cl"),
>                        (18,"Ar"),(19,"K"),(20,"Ca"),(21,"Sc"),
>                        (22,"Ti"),(23,"V"),(24,"Cr"),(25,"Mn"),
>                        (26,"Fe"),(27,"Co"),(28,"Ni"),(29,"Cu"),
>                        (30,"Zn"),(31,"Ga"),(32,"Ge"),(33,"As"),
>                        (34,"Se"),(35,"Br"),(36,"Kr"),(37,"Rb"),
>                        (38,"Sr"),(39,"Y"),(40,"Zr"),(41,"Nb"),
>                        (42,"Mo"),(43,"Tc"),(44,"Ru"),(45,"Rh"),
>                        (46,"Pd"),(47,"Ag"),(48,"Cd"),(49,"In"),
>                        (50,"Sn"),(51,"Sb"),(52,"Te"),(53,"I"),
>                        (54,"Xe"),(55,"Cs"),(56,"Ba"),(57,"La"),
>                        (58,"Ce"),(59,"Pr"),(60,"Nd"),(61,"Pm"),
>                        (62,"Sm"),(63,"Eu"),(64,"Gd"),(65,"Tb"),
>                        (66,"Dy"),(67,"Ho"),(68,"Er"),(69,"Tm"),
>                        (70,"Yb"),(71,"Lu"),(72,"Hf"),(73,"Ta"),
>                        (74,"W"),(75,"Re"),(76,"Os"),(77,"Ir"),
>                        (78,"Pt"),(79,"Au"),(80,"Hg"),(81,"Tl"),
>                        (82,"Pb"),(83,"Bi"),(84,"Po"),(85,"At"),(86,"Rn"),
>                        (87,"Fr"),(88,"Ra"),(89,"Ac"),(90,"Th"),
>                        (91,"Pa"),(92,"U"),(93,"Np"),(94,"Pu"),
>                        (95,"Am"),(96,"Cm"),(97,"Bk"),(98,"Cf"),
>                        (99,"Es"),(100,"Fm"),(101,"Md"),(102,"No"),
>                        (103,"Lr"),(104,"Rf"),(105,"Db"),(106,"Sg"),
>                        (107,"Bh"),(108,"Hs"),(109,"Mt"),(110,"Ds"),
>                        (111,"Rg"),(112,"Cn"),(113,"Uut"),(114,"Fl"),
>                        (115,"Uup"),(116,"Lv"),(117,"Uus"),(118,"Uuo")]

>number_to_name = maybe undefined id . flip lookup periodic_table
>periodic_table_names = map number_to_name [0..118]

>periods = [[1,2],[3..10],[11..18],[19..36],[37..54],[55..86],[87..118]]
>periods_names = map (map number_to_name) periods

>groups = [group1,group2,(group3 ++ lanthanides ++ actinides),group4,group5,group6,group7,group8,group9,group10,group11,group12,group13,group14,group15,group16,group17,group18]
>groups_names = map (map number_to_name) groups

>group1 =    [1,3,11,19,37,55,87]
>group2 =      [4,12,20,38,56,88]
>group3 =           [21,39]
>group4 =           [22,40,72,104]
>group5 =           [23,41,73,105]
>group6 =           [24,42,74,106]
>group7 =           [25,43,75,107]
>group8 =           [26,44,76,108]
>group9 =           [27,45,77,109]
>group10 =          [28,46,78,110]
>group11 =          [29,47,79,111]
>group12 =          [30,48,80,112]
>group13 =    [5,13,31,49,81,113]
>group14 =    [6,14,32,50,82,114]
>group15 =    [7,15,33,51,83,115]
>group16 =    [8,16,34,52,84,116]
>group17 =    [9,17,35,53,85,117]
>group18 = [2,10,18,36,54,86,118]

>nats :: Stream Integer
>nats = naturals

>-- | <http://en.wikipedia.org/wiki/Lanthanide>
>lanthanides = [57..71]
>actinides = [89..103]

>-- | <https://en.wikipedia.org/wiki/Quantum_number>

>angular_momentum = fmap (\l -> l * (l+1)) naturals

>principalQN :: Stream Integer
>principalQN = nonzero_naturals
>azimuthalQN :: Stream [Integer]
>azimuthalQN = fmap (\n -> [0..n-1]) principalQN
>azimuthalQN_ = codiagonals $ Matrix $ fmap constant naturals
>
>angularMomentum = fmap (map (\l -> l * (l + 1))) azimuthalQN
>
>magneticQN :: Stream [[Integer]]
>magneticQN = fmap (map (\l -> [-l..l])) azimuthalQN

>data Electron = Electron {
>  principal_quantum_number :: Integer,
>  azimuthal_quantum_number :: Integer,
>  magnetic_quantum_number  :: Integer,
>  spin_quantum_number_times_two  :: Integer }
>  deriving (Read)

>orbital_name :: Integer -> String
>orbital_name 0 = "s"
>orbital_name 1 = "p"
>orbital_name 2 = "d"
>orbital_name 3 = "f"
>orbital_name i = chr (ord 'g' + (fromIntegral i-4)) : []

>instance Show Electron where
>  show (Electron p a m s) = "Electron(" ++ show p ++ "," ++ show (orbital_name a) ++ "," ++ show m ++ "," ++ show ((fromIntegral s :: Rational)/2) ++ ")"

>instance PpShow Electron where
>  pp (Electron p a m s) = (pp "Electron(" <> pp p <> pp ",") <+> (pp (orbital_name a) <> pp ",") <+> (pp m <> pp ",") <+> (pp ((fromIntegral s :: Rational)/2) <> pp " )")

>instance PpShow [[[Electron]]] where
>  pp = ppf . map ppf . map (map ppf)
>instance PpShow [[Electron]] where
>  pp = ppf . map ppf

>instance PpShow [Electron] where
>  pp = ppf

>electrons = do p <- nonzero_naturals
>               return $ [[[Electron p a m 1,Electron p a m (negate 1)]
>                         | m <- [-a..a]]
>                        | a <- [0..p-1]] 

electrons = (\ (p,a,m) ->
               (\m' -> 
                  liftA2 (\m'' a' -> Electron p a' m'' (1%2)) m' a
               ) <$> m
            ) <$> pamQN

>pamQN :: Stream (Integer, [Integer], [[Integer]])
>pamQN = liftA3 (,,) principalQN azimuthalQN magneticQN

>pamElectrons :: Stream [Electron]
>pamElectrons = do (p,alst,mlstlst) <- pamQN
>                  return $ do
>                     a <- alst
>                     mlst <- mlstlst
>                     m <- mlst
>                     s <- [1,negate 1]
>                     return $ Electron p a m s
