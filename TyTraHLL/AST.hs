module TyTraHLL.AST

data Type = Vec Type Int | Tuple [Type] | Prim PrimType

data PrimType = Float | Double | Short | Char | Fix Int Int

data Action = 
    MOpaque Name [Expr] Type Type (Perf,Cost) | 
    FOpaque Assoc Name [Expr] Expr Type Type (Perf,Cost) |
    PNDMap [Int] Action |
    PNDFold [Int] Action Expr | 
    NDMap [(Int, MVariant)] MVariant Action |
    NDFold [(Int, FVariant)] Action Expr | 
    NDSplit [Int] | 
    NDMerge [Int] | 
    NDDistr [Int] [Int] | 
    NDZipT [Int] [Type] | 
    NDUnzipT [Int] Type |
    Compose [Action] | 
    Lambda [Expr] Action | Let [Assignment] Expr
    Loop Int Action

data Expr = Var Name Type | Res Action Expr | Tup Expr
data Assignment = Assign Expr Expr

data MVariant = Par | Pipe | Seq
data FVariant = Tree | Pipe | Seq

type TyTraHLLProgram = Action
