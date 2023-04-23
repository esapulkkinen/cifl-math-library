>{-# LANGUAGE LambdaCase #-}
>module Physics.Particles where
>import Data.Ratio
>import GHC.Real
>import qualified Math.Number.Stream as Stream
>import qualified Math.Number.StreamInterface as StreamI
> 
>-- | <https://en.wikipedia.org/wiki/Particle_zoo>
>-- | <https://en.wikipedia.org/wiki/List_of_particles>
>data Quark = UpQuark | DownQuark
>           | StrangeQuark | CharmQuark
>           | BottomQuark | TopQuark
>           | AntiUpQuark | AntiDownQuark
>           | AntiStrangeQuark | AntiCharmQuark
>           | AntiBottomQuark  | AntiTopQuark
>  deriving (Eq, Show)

>data Lepton = Electron | Positron
>            | Muon | AntiMuon
>            | Tau | AntiTau
>   deriving (Eq,Show)
>data Neutrino = Neutrino Lepton
>   deriving (Eq,Show)
>data Boson = Photon | WBoson | ZBoson | HiggsBoson | Gluon | Graviton
>   deriving (Eq,Show)
>data Hadron = MesonHadron Meson | BaryonHadron Baryon
>   deriving (Eq,Show)

>data Baryon = Baryon Quark Quark Quark
>  deriving (Eq,Show)
> 
>data Meson  = Meson Quark Quark
>  deriving (Eq, Show)

>data Fermion = BaryonFermion Baryon
>             | LeptonFermion Lepton
>  deriving (Eq,Show)
> 
>data Spin = HalfSpin | ThreeHalvesSpin
>          | MinusHalfSpin | MinusThreeHalvesSpin
>          | ZeroSpin | OneSpin | TwoSpin
>          | MinusOneSpin | MinusTwoSpin
>   deriving (Eq,Show)

>data QuarkColor = Colorless | Red | Green | Blue | AntiRed | AntiGreen | AntiBlue
>  deriving (Eq,Show)
>data GluonColor = GluonColor QuarkColor QuarkColor
>  deriving (Eq,Show)
>data IsoSpin = HalfIsoSpin | ThreeHalvesIsoSpin | OneIsoSpin | ZeroIsoSpin
>             | MinusHalfIsoSpin | MinusThreeHalvesIsoSpin | MinusOneIsoSpin
>  deriving (Eq,Show)
>data Charge = ChargeMinusTwo | ChargeMinusOne | ChargeZero | ChargeOne | ChargeTwo
>            | ChargeOneThird | ChargeTwoThirds | ChargeMinustOneThird | ChargeMinusTwoThirds
>   deriving (Eq,Show)
>data Generation = GenerationOne | GenerationTwo | GenerationThree
>   deriving (Eq,Show)
>data Mass = Mass { meV_per_c2 :: Double }
>   deriving (Eq,Show)
>class Particle p where
>   particle_spin :: p -> Spin
>   particle_isospin :: p -> IsoSpin
>   particle_charge :: p -> Charge
>   particle_mass :: p -> Mass
>   particle_strangeness :: p -> Int
>   particle_charmness :: p -> Int
>   particle_top_quarks :: p -> Int
>   particle_bottomness :: p -> Int
>   particle_antiparticle :: p -> p
>   particle_generation :: p -> Generation


>antiquark :: Quark -> Quark
>antiquark = \case
>  UpQuark -> AntiUpQuark
>  DownQuark -> AntiDownQuark
>  StrangeQuark -> AntiStrangeQuark
>  CharmQuark -> AntiCharmQuark
>  BottomQuark -> AntiBottomQuark
>  TopQuark -> AntiTopQuark
>  AntiUpQuark -> UpQuark
>  AntiDownQuark -> DownQuark
>  AntiStrangeQuark -> StrangeQuark
>  AntiCharmQuark -> CharmQuark
>  AntiBottomQuark -> BottomQuark
>  AntiTopQuark -> TopQuark

>antilepton :: Lepton -> Lepton
>antilepton = \case
>   Electron -> Positron
>   Muon -> AntiMuon
>   Tau -> AntiTau
>   Positron -> Electron
>   AntiMuon -> Muon
>   AntiTau -> Tau

>data BaryonOctet = BaryonOctet {
>  positive_nostrange :: Baryon,
>  positive_strange :: Baryon,
>  neutral_nostrange :: Baryon,
>  neutral_strange_sigma :: Baryon,
>  neutral_strange_lambda :: Baryon,
>  neutral_doubly_strange :: Baryon,
>  negative_strange :: Baryon,
>  negative_doubly_strange :: Baryon }
>  deriving (Eq,Show)
>
>data BaryonDecuplet = BaryonDecuplet {
>   decuplet_octet :: BaryonOctet,
>   negative_nostrange :: Baryon,
>   negative_triple_strange :: Baryon,
>   doubly_positive_neutral :: Baryon }
>  deriving (Eq,Show)

>proton = Baryon UpQuark UpQuark DownQuark
>neutron = Baryon UpQuark DownQuark DownQuark
>sigma_minus = Baryon DownQuark DownQuark StrangeQuark
>sigma_plus  = Baryon UpQuark UpQuark StrangeQuark
>sigma_zero  = Baryon UpQuark DownQuark StrangeQuark
>strange_lambda      = Baryon UpQuark DownQuark StrangeQuark
>xi_minus = Baryon DownQuark StrangeQuark StrangeQuark
>xi_zero = Baryon UpQuark StrangeQuark StrangeQuark

>spin_half_baryon_octet :: BaryonOctet
>spin_half_baryon_octet =
>  BaryonOctet proton sigma_plus neutron sigma_zero strange_lambda xi_zero
>              sigma_minus xi_minus

>quark_isospin :: Quark -> IsoSpin
>quark_isospin UpQuark = HalfIsoSpin
>quark_isospin DownQuark = HalfIsoSpin
>quark_isospin _ = ZeroIsoSpin

>baryon_isospin :: Baryon -> IsoSpin
>baryon_isospin (Baryon a b c) =
>   quark_isospin a `isospin_plus`
>   quark_isospin b `isospin_plus`
>   quark_isospin c

>isospin_plus :: IsoSpin -> IsoSpin -> IsoSpin
>isospin_plus a b = rational_isospin (isospin_rational a + isospin_rational b)

>isospin_rational :: IsoSpin -> Rational
>isospin_rational HalfIsoSpin = 1%2
>isospin_rational ThreeHalvesIsoSpin = 3%2
>isospin_rational OneIsoSpin = 1%1
>isospin_rational ZeroIsoSpin = 0%1
>isospin_rational MinusHalfIsoSpin = -1%2
>isospin_rational MinusThreeHalvesIsoSpin = -3%2
>isospin_rational MinusOneIsoSpin = -1%1

>rational_isospin :: Rational -> IsoSpin
>rational_isospin (1 :% 2) = HalfIsoSpin
>rational_isospin (3 :% 2) = HalfIsoSpin
>rational_isospin (1 :% 1) = HalfIsoSpin
>rational_isospin (0 :% 1) = HalfIsoSpin
>rational_isospin (-1 :% 2) = HalfIsoSpin
>rational_isospin (-3 :% 2) = HalfIsoSpin
>rational_isospin (-1 :% 1) = HalfIsoSpin
>rational_isospin _ = error "invalid isospin"

>data ElectronQN = ElectronQN {
>   principal_qn :: Integer,
>   azimuthal_qn :: Integer,
>   magnetic_qn :: Integer,
>   spin_qn :: Rational
> }
>  deriving (Eq)
>
>instance Show ElectronQN where
>   show z@(ElectronQN {}) = show (principal_qn z)
>                          ++ (azi_letter !! fromIntegral (azimuthal_qn z))
>                          : show (magnetic_qn z)
>                          ++ (spin_letter !! fromIntegral (1+Data.Ratio.numerator (spin_qn z))) : []
>     where azi_letter = "spdfghiklmnopqrstuvwxyz"
>           spin_letter = "s0S"
> 

>particles :: StreamI.Stream ElectronQN
>particles = Stream.cycle $ do
>   pn <- [1..]
>   pl <- [0..pn-1]
>   ml <- [-pl..pl]
>   s <- [-1%2, 1%2]
>   return $ ElectronQN pn pl ml s
