{-# LANGUAGE DeriveDataTypeable #-}
module AST where

import           Data.List
import           Data.Data
import           Data.Generics
import           Data.Typeable
import           Data.Generics.Uniplate.Data

-- placeholders


-- Symbolic types
data SType = SVec SType [Integer] | SAtom Type | SAny deriving (Data, Typeable, Show, Eq, Ord)


data Name = MkName String deriving (Data, Typeable, Show, Eq, Ord)
data Perf = MkPerf Integer deriving (Data, Typeable, Show, Eq, Ord)
data Assoc = MkAssoc Bool deriving (Data, Typeable, Show, Eq, Ord)

data Type = Vec Type Integer | Tuple [Type] | Prim PrimType -- | NDVec Type [Integer]
  deriving (Data, Typeable,  Eq, Ord)

--
instance Show Type where
  show (Vec ty sz) = show sz ++ " " ++show ty ++ " "
  show (Tuple tys) = "Tuple " ++ show tys
  show (Prim ty) = show ty

data PrimType = Float | Double | Short | Char | Fix Integer Integer
  deriving (Data, Typeable, Show, Eq, Ord)
-- Add variants everywhere
-- NDSplit has MVariant or FVariant
-- [Integer] list is replicated where needed (each subtree individually costable)
-- split -> map -> merge
data Action =
{-
    MOpaque Name [Expr] Type Type (Perf,Cost) |
    FOpaque Assoc Name [Expr] Expr Type Type (Perf,Cost) |
    PNDMap [(Integer, MVariant)] Action |
    PNDFold [(Integer, FVariant)] Action |
    NDMap [(Integer, MVariant)] MVariant Action |
    NDFold [(Integer, FVariant)] Action |
    NDSplit [(Integer,Variant)] |
    NDMerge [(Integer, Variant)] |
    NDDistr [(Integer,Variant)] [(Integer,Variant)] |
    NDZipT [Integer] Type | -- output type rather than input to keep things simple
    NDUnzipT [Integer] Type | -- out is going to be a tuple of lists so single type
-}
    MOpaque Name [Expr] Type Type RawCost |
    FOpaque Assoc Name [Expr] Expr Type Type RawCost |
    PNDMap [Integer] Action |
    PNDFold [Integer] Action |
    NDMap [(Integer, MVariant)] MVariant Action |
    NDFold [(Integer, FVariant)] FVariant Action |
    NDSplit [Integer] |
    NDMerge [Integer] |
    NDDistr [Integer] [Integer] |
    NDZipT [Integer] | -- output type rather than input to keep things simple
    NDUnzipT [Integer] | -- out is going to be a tuple of lists so single type

    Compose [Action] |
    Let Expr Expr | -- lhs is variable, rhs is what it is, within outer Res
    Loop Integer Integer Integer Action
    deriving (Data, Typeable, Show, Eq, Ord)
    --Empty -- Placeholder

data Expr =
  Var Name Type
  | Res Action Expr
  | Tup [Expr]
  deriving (Data, Typeable, Show, Eq, Ord)
  --  | EmptyExpr -- placeholder

-- instance Show Expr where
--   show (Var (MkName name) ty) = "$" ++ name ++ ":" ++ show ty
--   show (Res action input) =  " <= " ++ show action ++ " <- " ++ show input
--   show (Tup exprs) = "Tup " ++ show exprs


sizeTy :: Type -> Integer
sizeTy (Vec ty sz) = sz * sizeTy ty
sizeTy (Tuple xs) = 1
sizeTy (Prim _) = 1


ndVecDim :: Type -> [Integer]
ndVecDim (Vec ty sz) = sz : ndVecDim ty
ndVecDim e = []

isAssoc :: Action -> Bool
isAssoc (FOpaque (MkAssoc assoc) _ _ _ _ _ _ ) = assoc
isAssoc (PNDMap _ iact) = isAssoc iact
isAssoc (NDMap _ _ iact) = isAssoc iact
isAssoc (Loop _ _ _ iact) = isAssoc iact
isAssoc (Compose actions) = all isAssoc actions
isAssoc _ = False


-- the fact that the LHS is also an expr is a problem for "everywhere" transformations
data Assignment = Assign Expr Expr
  deriving (Data, Typeable, Show, Eq)

data Variant = MVar MVariant | FVar FVariant
data MVariant = Par | Pipe | Seq deriving (Data, Typeable, Show, Eq, Ord)
data FVariant = Tree | FPipe | FSeq deriving (Data, Typeable, Show, Eq, Ord)

type TyTraHLLProgram = Assignment -- Assignment



-- -For costing




-- dsp's , luts, blockrams
-- clock domains
-- perf : tdel / tlat
-- 600 mhz / critical path * ram banks * words /cycle * 2 ( up and down )
type Bit = Integer
type Cycle = Integer

data RawCost = MkRawCost {
  delay :: Cycle -- Number of cycles taken to compute one unit
  ,latency :: Cycle -- Number of cycles to begin computation
  ,size :: ResourceUse -- Number of units of circuitry ( LUTs ) taken up
} deriving (Data, Eq,  Ord)

instance Show RawCost where
  show (MkRawCost { delay = d, latency = l, size = sz}) = "Cost " ++ show d ++ " del, " ++ show l ++ " lat, " ++ show sz

data CostTree = CostScalar RawCost | CostVec Integer CostTree deriving (Data, Typeable, Show, Eq, Ord)
data CostVariant = CPipe | CPar | CSeq | CTree deriving (Data, Typeable, Show, Eq, Ord)

instance Num RawCost where
  (+)
    MkRawCost { delay = d1, latency = l1, size = s1}
    MkRawCost { delay = d2, latency = l2, size = s2}
      = MkRawCost { delay = d1 + d2, latency = l1 + l2, size = s1 + s2 }
  (-)
    MkRawCost { delay = d1, latency = l1, size = s1}
    MkRawCost { delay = d2, latency = l2, size = s2}
      = MkRawCost { delay = d1 - d2, latency = l1 - l2, size = s1 - s2 }
  (*)
    MkRawCost { delay = d1, latency = l1, size = s1}
    MkRawCost { delay = d2, latency = l2, size = s2}
      = MkRawCost { delay = d1 * d2, latency = l1 * l2, size = s1 * s2 }

  fromInteger jnt = MkRawCost { delay = jnt, latency =  jnt, size = fromInteger jnt}
  signum MkRawCost { delay = d1, latency = l1, size = s1} = MkRawCost { delay = signum d1, latency = signum l1, size = signum s1}
  abs MkRawCost { delay = d1, latency = l1, size = s1} = MkRawCost { delay = abs d1, latency = abs l1, size = abs s1}


data ResourceUse = MkResourceUse {
  luts :: Integer
  , blockRams :: Integer
  , multipliers :: Integer
} deriving (Data, Eq,  Ord)

instance Show ResourceUse where
  show (MkResourceUse {luts = l, blockRams = r, multipliers = m}) = "Res:" ++ show l ++ " luts," ++ show r ++ "ram," ++ show m ++ " mult "

instance Num ResourceUse where
  (+)
    MkResourceUse { luts = l1, blockRams = r1, multipliers = m1 }
    MkResourceUse { luts = l2, blockRams = r2, multipliers = m2 }
      = MkResourceUse { luts = l1 + l2, blockRams = r1 + r2, multipliers = m1 + m2}
  (-)
    MkResourceUse { luts = l1, blockRams = r1, multipliers = m1 }
    MkResourceUse { luts = l2, blockRams = r2, multipliers = m2 }
      = MkResourceUse { luts = l1 - l2, blockRams = r1 - r2, multipliers = m1 - m2}
  (*)
    MkResourceUse { luts = l1, blockRams = r1, multipliers = m1 }
    MkResourceUse { luts = l2, blockRams = r2, multipliers = m2 }
      = MkResourceUse { luts = l1 * l2, blockRams = r1 * r2, multipliers = m1 * m2}

  fromInteger jnt = MkResourceUse { luts = jnt, blockRams = jnt, multipliers = jnt }
  signum   MkResourceUse { luts = l1, blockRams = r1, multipliers = m1 } = MkResourceUse { luts = signum l1, blockRams = signum r1, multipliers = signum m1}
  abs   MkResourceUse { luts = l1, blockRams = r1, multipliers = m1 } = MkResourceUse { luts = abs l1, blockRams = abs r1, multipliers = abs m1}
