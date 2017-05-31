module Inference where

import AST
import           Debug.Trace(trace)






--class Inferable a where
--  inferType :: a -> Type

--instance Inferable Expr where
inferType :: Expr -> Type
inferType expr = case expr of
  (Var name ty) -> ty
  (Res action input) ->
    let result = typeOfAction action $ inferType input
        in do
          --Debug.Trace.trace ("act: " ++ show action ++ " \n\t " ++ show input ++ " \n\t -> " ++ show  result ) $
          result
  (Tup exprs) ->
    case exprs of
      [] -> Tuple []
      _ -> Tuple (map inferType exprs)

unwrapVecFull ::  Type -> Type
unwrapVecFull ty = case ty of
    (Vec innerTy ix) -> unwrapVecFull innerTy
    e-> e

wrapVec :: [Integer] -> Type -> Type
wrapVec [] ty = ty
wrapVec (x:xs) ty =  Vec (wrapVec xs ty) x


unwrapVecTy :: [Integer] -> Type -> Type
--unwrapVecTy [] (Vec innTy sz) = innTy
unwrapVecTy [] e = e -- error "could not fully unwrapp input"
unwrapVecTy (x:xs) (Vec innTy sz) = if x == sz then unwrapVecTy xs innTy else error "dimensions did not match while unwrapping"
unwrapVecTy (x:xs) e = error "could not fully unwrapp input"

typeOfAction :: Action -> (Type -> Type)
typeOfAction action@(MOpaque name extra inTy outTy rawCost) =
  \inSTy ->   -- Debug.Trace.trace ("action: " ++ show action ++ " " ++ show inSTy) $
    if inSTy == inTy then outTy else error $ "yous done goofed " ++ show inSTy ++ " exp: " ++ show inTy

typeOfAction action@(FOpaque assoc name extra acc inTy outTy rawCost) =
  \inSTy -> -- Debug.Trace.trace ("action: " ++ show action ++ " " ++ show inSTy) $
    if inSTy == inTy then outTy else error $ "you folded goofed" ++ show inSTy ++ " exp: " ++ show inTy

typeOfAction action@(PNDMap [] innerAction) =
  \inSTy -> -- Debug.Trace.trace ("action: " ++ show action ++ " " ++ show inSTy) $
        case inSTy of
          (Vec innerInTy sz) -> Vec (typeOfAction innerAction innerInTy) sz
          other -> error $ "you map goofed " ++ show inSTy

typeOfAction action@(PNDMap fctrs@(x:xs) innerAction) =
  \inSTy -> -- Debug.Trace.trace ("action: " ++ show action ++ " " ++ show inSTy) $
    case inSTy of
      (Vec innerInTy sz) -> if x == sz
        then Vec (typeOfAction (PNDMap xs innerAction) innerInTy) sz
        else error "dimensions did not match in map"
      _ ->  error "not a vector "




typeOfAction action@(PNDFold [] innerAction) =
  \inSTy ->  -- Debug.Trace.trace ("action: " ++ show action ++ " " ++ show inSTy) $
    case inSTy of
      (Vec innerInTy sz) -> typeOfAction innerAction innerInTy
      other -> error $ "Fold faild " ++ show inSTy

typeOfAction action@(PNDFold fctrs@(x:xs) innerAction) =
  \inSTy -> -- Debug.Trace.trace ("action: " ++ show action ++ " " ++ show inSTy) $
    case inSTy of
      (Vec innerInTy sz) ->
       if (sz == x)
         then typeOfAction (PNDFold xs innerAction) innerInTy
         else error $ "Fold faild  x" ++ show inSTy
      other -> error $ "Fold faild " ++ show inSTy
  --
  -- case inSTy of
  --   (Vec innerInTy sz) ->
  --   if x == sz
  --     then Vec (typeOfAction (PNDFold xs innerAction) innerInTy) sz
  --     else error "dimensions did not match in fold"
  --   _ ->  error "not a vector f"

typeOfAction (NDMap fctrs _ iact) = typeOfAction (PNDMap (map fst fctrs) iact)
typeOfAction (NDFold fctrs _ iact) = typeOfAction (PNDFold (map fst fctrs) iact)

typeOfAction action@(Compose []) = id
typeOfAction action@(Compose (x:xs)) = --Debug.Trace.trace ("\n\n¬! " ++ show action ) $
 typeOfAction x . typeOfAction (Compose xs)

typeOfAction (NDSplit []) = id
typeOfAction (NDSplit fctrs@(x:xs)) =
  \inSTy -> case inSTy of
    (Vec innerTy sz) -> if sz `mod` product fctrs == 0
      then let k = sz `div` product fctrs -- Debug.Trace.trace ("a1! " ++ show  ( wrapVec fctrs (Vec innerTy k)) ) $
              in  wrapVec fctrs (Vec innerTy k)
      else error $ "could not split " ++ show inSTy ++ " " ++ show fctrs
    _ -> error "can't split nonvector"



--
typeOfAction (NDMerge []) = id
  -- \inSTy ->  let fullSize = sizeTy inSTy
  --                in
  --                   (Vec (unwrapVecFull inSTy) fullSize)

-- typeOfAction (NDMerge []) =
--   \inSTy ->
--     case inSTy of
--       (Vec innerTy sz) -> case innerTy of
--         (Vec inner2Ty sz2) -> Vec inner2Ty (sz * sz2)
--         _ -> error "messed up merge"
--       _ -> error "messed up merge"

typeOfAction (NDMerge fctrs@(x:xs)) =
  \inSTy -> -- Debug.Trace.trace ("merge: " ++ show fctrs ++ " in " ++ show inSTy) $
   case inSTy of
    (Vec innerTy sz) ->
      if x == sz
        then  if xs == []
              then case innerTy of
                      (Vec in2ExprTy sz2) -> Vec in2ExprTy $ sz2 * sz
                      e -> (Vec innerTy sz)
               else case typeOfAction (NDMerge xs) innerTy of
                 (Vec in2ExprTy sz2) -> Vec in2ExprTy $ sz2 * sz
                 e -> error "wut ndmerge"
        else  error $ "come up with better error messages - NdMerge: "
    e -> error $ "come up with better error messages - NdMerge x2 " ++ show inSTy
-- here

typeOfAction (NDDistr sFctrs mFctrs) = typeOfAction (Compose [NDSplit sFctrs, NDMerge mFctrs])



typeOfAction (NDZipT fctrs ) =
  \inSTy -> case inSTy of
    (Tuple [])  -> error "can't zip empty types"
    (Tuple (x:xs)) ->
     if all (sameSize x) xs
      then typeOfAction (NDSplit fctrs) $ buildV (x:xs)
      else error " zip applied to nonuniform vec"
      where
        sameSize :: Type -> Type -> Bool
        sameSize (Vec _ szl) (Vec _ szr) = szl == szr
        buildV :: [Type] -> Type
        buildV tys = let (Vec _ sz) = head tys in
          Vec (Tuple (extractTys tys)) sz where
          extractTys :: [AST.Type] -> [AST.Type]
          extractTys [] = []
          extractTys (y:ys) = case y of
            (Vec ty _) -> ty : extractTys ys
            _ -> error "expecting tuple of vectors and got something else in zip"

--typeOfAction (NDUnzipT [] ) = id
typeOfAction (NDUnzipT fctrs)=
  \inSTy -> -- Debug.Trace.trace ("\n\n¬! " ++ show inSTy ) $
    case typeOfAction (NDMerge fctrs) inSTy of
      (Vec (Tuple tys) sz) -> newty tys
        where
          newty :: [AST.Type] -> AST.Type
          newty [] = error "no inner types for unzip"
          newty tys = AST.Tuple ( map (`AST.Vec` sz) tys)
      _ -> error "needs factprs for unzip"


typeOfAction e  = id



inferCheck :: Assignment -> Type
inferCheck (Assign lhs rhs) = inferType rhs
