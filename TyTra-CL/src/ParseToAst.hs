module ParseToAst where

import qualified AST
import qualified Cost
import qualified Data.Map         as Map
import qualified ParseTree        as PT
import           Inference

import           System.IO
import           System.IO.Unsafe

import qualified          Debug.Trace(trace)


type TypeContext = Map.Map PT.Name ([PT.Type], Bool)

-- type ContextState = State TypeContext





extractTypeDecl :: PT.Program -> TypeContext
extractTypeDecl (PT.MkProgram tys _ ) = Map.fromList $ map ( \(PT.MkTypeDecl name ty assoc) -> (name, (ty, assoc)) )  tys


trace _ = id


transTy :: PT.Type -> AST.Type
transTy (PT.Vec ty sz) = AST.Vec (transTy ty) sz
transTy (PT.Tuple tys) = AST.Tuple (map transTy tys)
transTy (PT.Prim pty) = case pty of
  PT.Float -> AST.Prim AST.Float
  PT.Double -> AST.Prim AST.Double
  PT.Short -> AST.Prim AST.Short
  PT.Char -> AST.Prim AST.Char
  (PT.Fix intg dec) -> AST.Prim (AST.Fix intg dec)

getTypeOf :: TypeContext -> PT.Name -> [PT.Type]
getTypeOf tyc name =
      case Map.lookup name tyc of
        (Just tys) -> case tys of
          ([],_) -> error "nae types"
          (ttys,_) -> ttys
        Nothing -> error $ "Variable not bound" ++ show name

ptToAstName :: PT.Name -> AST.Name
ptToAstName name =  case name of (PT.MkName ptname) -> AST.MkName ptname

isAssoc :: TypeContext -> PT.Name -> Bool
isAssoc tyc name =
  let astName = ptToAstName name
      tTy = Map.lookup name tyc
    in
      case tTy of
        (Just tys) -> case tys of
          (_,assoc) -> assoc
        Nothing -> error $ "Variable not bound:" ++ show name

inType :: TypeContext -> PT.Name -> AST.Type
inType tyc name = transTy $ head (getTypeOf tyc name)

outType :: TypeContext -> PT.Name -> AST.Type
outType tyc name = transTy $ last (getTypeOf tyc name)

class Transformable a where
  transform :: TypeContext -> a -> AST.Expr

class TransformableAction a where
  transformAction :: TypeContext -> a -> AST.Action

instance TransformableAction PT.FunctionOccurence where
  transformAction tyc (PT.MkMapOccurence funOcc) = AST.PNDMap [] (transformAction tyc funOcc)
  transformAction tyc (PT.MkFoldOccurence funOcc acc) = let (PT.MkFunctionOccurence name extra) = funOcc in
       AST.PNDFold [] (AST.FOpaque (AST.MkAssoc $ isAssoc tyc name) (ptToAstName name) (map (transform tyc) extra) (transform tyc acc) (inType tyc name) (outType tyc name) Cost.defaultRawCost)
  transformAction tyc (PT.MkLoopOccurence start stop step action) =
    AST.Loop start stop step (transformAction tyc action)
  transformAction tyc (PT.MkFunctionOccurence name extra) =
    AST.MOpaque (ptToAstName name) (map (transform tyc) extra) (inType tyc name) (outType tyc  name) Cost.defaultRawCost
  transformAction tyc (PT.MkCompose actions) = AST.Compose (map (transformAction tyc) actions)

instance Transformable PT.LetLhs where
  transform tyc (PT.MkLetLhs names) = case length names of
    0 -> error "w2hat"
    1 -> AST.Var (ptToAstName (head names)) (transTy $ head (getTypeOf tyc (head names)))
    _ -> AST.Tup (map (transform tyc . PT.MkVariable) names)

instance Transformable PT.ResultsInValue where
  transform tyc riv = case riv of
    (PT.MkVariable name) -> AST.Var (ptToAstName name) (transTy $ head (getTypeOf tyc name))
    (PT.MkFunctionApplication funOcc input) -> AST.Res (transformAction tyc funOcc) (transform tyc input)
    (PT.MkLet [] chain) -> transform tyc chain
    (PT.MkLet (x:xs) chain) ->
      let (PT.MkLetAssignment lhs rhs) = x in
        AST.Res (AST.Let (transform tyc lhs) (transform tyc (PT.MkLet xs chain))) (transform tyc rhs)
    -- something fishy here [a,b] means Vec Tup(a, b) = Res  Zip   Tup (Vec a, Vec b)
    (PT.MkZip exprs) ->
      let   exprInner = map (transform tyc) exprs
            exprTys = map inferType exprInner
            in
              case exprTys of
                [] -> error "empty thingys passed to zip"
                (x:xs) -> if all (sameSize x) xs
                  then AST.Res (AST.NDZipT [] ) (AST.Tup exprInner) --  $ buildV exprTys
                  else error " zip applied to nonuniform vec"
                  where
                    sameSize :: AST.Type -> AST.Type -> Bool
                    sameSize (AST.Vec _ szl) (AST.Vec _ szr) = szl == szr
                    -- buildV :: [AST.Type] -> AST.Type
                    -- buildV tys = let (AST.Vec _ sz) = head tys in
                    --   AST.Vec (AST.Tuple (extractTys tys)) sz where
                    --   extractTys :: [AST.Type] -> [AST.Type]
                    --   extractTys [] = []
                    --   extractTys (y:ys) = case y of
                    --     (AST.Vec ty _) -> ty:(extractTys ys)
                    --     _ -> error "expecting tuple of vectors and got something else in zip"
    -- unzip :: [(a, b)] -> ([a], [b])
    (PT.MkUnzip input) ->
      let   exprInner = transform tyc input
            exprTy = inferType exprInner
            in
              case exprTy of
                AST.Vec (AST.Tuple innerTys) sz -> AST.Res (AST.NDUnzipT [] ) exprInner where -- (newty innerTys)
                  newty :: [AST.Type] -> AST.Type
                  newty [] = error "no inner types for unzip"
                  newty tys = AST.Tuple ( map (`AST.Vec` sz) tys)
                _ -> error $ "unzip type unexpected" ++ show exprTy ++ " input" ++ show exprInner

tKernel :: TypeContext -> PT.LetAssignment -> AST.TyTraHLLProgram
tKernel tyc (PT.MkLetAssignment lhs rhs) = AST.Assign (transform tyc lhs) (transform tyc rhs)

trfm :: PT.Program -> AST.TyTraHLLProgram
trfm program@(PT.MkProgram tys krnl) =
  let
    declMap = extractTypeDecl program
  in
    tKernel declMap krnl
