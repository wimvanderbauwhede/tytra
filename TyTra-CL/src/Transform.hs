{-# OPTIONS -Wall #-}
{-# LANGUAGE RankNTypes #-}
module Transform where

import qualified AST
import           Cost
import           Data.Generics.Uniplate.Data
import           Data.List
import qualified Debug.Trace                 (trace)
import           Inference
import           Zora.Math



genAllVar :: [Integer] -> [  ( [ (Integer, AST.MVariant )] , AST.MVariant)  ]
genAllVar [] = [  ([],AST.Seq)  ]
genAllVar fctrs =
  let pVars =  [AST.Pipe, AST.Par, AST.Seq]
      zr = mapM (const pVars) [1..(length fctrs)]
      in
        do
          pv <- pVars
          zz <- zr
          return (zip fctrs zz, pv)

genAllVarF :: [Integer] -> [  ( [ (Integer, AST.FVariant )] , AST.FVariant)  ]
genAllVarF [] = [  ([],AST.FSeq)  ]
genAllVarF fctrs =
  let pVars =  [AST.Tree, AST.FPipe, AST.FSeq]
      zr = mapM (const pVars) [1..(length fctrs)]
        in
          do
            pv <- pVars
            zz <- zr
            return (zip fctrs zz, pv)


mutate :: AST.Expr -> [AST.Expr]
mutate x = concat [ Prelude.map ( \(x,y) ->  gen $ (AST.Res (AST.NDMap x y iact) input )) (genAllVar fctrs)  | ((AST.Res (AST.PNDMap fctrs iact ) input), gen )<- contexts x ]

mutatef :: AST.Expr -> [AST.Expr]
mutatef x = concat [ Prelude.map ( \(x,y) ->  gen $ (AST.Res (AST.NDFold x y iact) input )) (genAllVarF fctrs)  | ((AST.Res (AST.PNDFold fctrs iact ) input), gen )<- contexts x ]


fz :: AST.Expr -> AST.Expr

fz node@(AST.Res (AST.PNDMap [] iact ) input) =  -- Debug.Trace.trace ("asdf" ++ show node) $
  case inferType input of
    (AST.Vec innerTy sz) -> fz $ Data.List.foldr (.) id ( Prelude.map (splitInputsBy) (init $ factor sz) )   $ node
    _ -> node

fz node@(AST.Res (AST.PNDFold [] iact ) input) = -- Debug.Trace.trace ("asdf" ++ show node) $
  if AST.isAssoc iact
    then
        case inferType input of
            (AST.Vec innerTy sz) -> fz $ Data.List.foldr (.) id ( Prelude.map (splitInputsBy) (init $ factor sz) )   $ node
            _ -> node
    else
        node
fz node@(AST.Res (AST.PNDMap fctrs@(x:xs) iact ) input) =
  let vars = genAllVar fctrs
      in
        let allVars = mutate node
            allCosts = Prelude.map computeExprCost allVars
            minCost = minimum allCosts
            lll = [ z | z <- mutate node , (computeExprCost z) <= minCost]
            in
              -- Debug.Trace.trace (show lll) $
              head lll
fz node@(AST.Res (AST.PNDFold fctrs@(x:xs) iact ) input) =
  let vars = genAllVarF fctrs
      in
        let allVars = mutatef node
            allCosts = Prelude.map computeExprCost allVars
            minCost = minimum allCosts
            lll = [ z | z <- mutatef node , (computeExprCost z) <= minCost]
            in
              -- Debug.Trace.trace (show lll) $
              head lll
fz e = e


inlineLets :: AST.Expr -> AST.Expr
inlineLets (AST.Res (AST.Let lhs@(AST.Var n m) chain) rhs) = transform rpl chain
  where
      rpl l@(AST.Var _ _) = if l == lhs then rhs else l
      rpl e               = e
inlineLets e = e


trComp :: AST.Expr -> AST.Expr
--trComp (AST.Res alpha@(AST.Compose xs) chain@(AST.Res (AST.Compose ixs) innerChain)) = AST.Res (AST.Compose (xs++ixs)) innerChain
trComp (AST.Res action chain@(AST.Res (AST.Compose xs) innerchain)) = AST.Res (AST.Compose (action:xs)) innerchain
trComp (AST.Res action chain@(AST.Res innerAction innerChain)) = AST.Res (AST.Compose (action:[innerAction])) innerChain
trComp e = e



-- FIXME: these will mess up higher-up nodes that have precomputed types
-- for downhill

-- Split

-- try a mark-sweep approach
-- value based things
roomForSplit :: [Integer] -> Integer -> Integer -> Bool
roomForSplit fctrs k sz = sz `mod` product (k:fctrs) == 0






colapsePND :: AST.Action -> AST.Action
colapsePND (AST.PNDMap f1 (AST.PNDMap f2 act))   = AST.PNDMap (f1 ++ f2) act
colapsePND (AST.PNDFold f1 (AST.PNDFold f2 act)) = AST.PNDFold (f1 ++ f2) act
colapsePND e                                     = e

-- Split MAPs into components





depth :: AST.Expr -> Int
depth = para (\_ cs -> 1 + maximum (0:cs))

{-

-}
splitInputsBy :: Integer -> AST.Expr -> AST.Expr
splitInputsBy k input@(AST.Res (AST.PNDMap [] action) node) =
  case inferType node of
    (AST.Vec inTy sz) -> if sz `mod` k == 0
      then AST.Res (AST.NDMerge [k]) $ AST.Res (AST.PNDMap [k] action) (AST.Res (AST.NDSplit [k]) node)
      else input
    e ->   input
splitInputsBy k input@(AST.Res (AST.NDMerge fctrm ) ( AST.Res (AST.PNDMap fctrmap action) (AST.Res (AST.NDSplit fctrs) node) ))=
  case inferType node of
    (AST.Vec inTy sz) -> if sz `mod` k == 0
      then AST.Res (AST.NDMerge (k:fctrm)) ( AST.Res (AST.PNDMap (k:fctrmap) action) (AST.Res (AST.NDSplit (k:fctrs)) node))
      else input
    e ->   input
splitInputsBy k input@(AST.Res (AST.PNDFold [] action) node) =
  case inferType node of
    (AST.Vec inTy sz) -> if sz `mod` k == 0 && AST.isAssoc action
      then AST.Res (AST.PNDFold [k] action) (AST.Res (AST.NDSplit [k]) node)
      else input
    e ->   input
splitInputsBy k input@(AST.Res (AST.PNDFold fctrmap action) (AST.Res (AST.NDSplit fctrs) node) )=
  case inferType node of
    (AST.Vec inTy sz) -> if sz `mod` k == 0 && AST.isAssoc action
      then AST.Res (AST.PNDFold (k:fctrmap) action) (AST.Res (AST.NDSplit (k:fctrs)) node)
      else input
    e ->   input
splitInputsBy k e = e





depthOp :: Int -> Int -> (AST.Expr -> AST.Expr) -> AST.Expr -> AST.Expr
depthOp mini maxi op  input =
  if (depth input >= mini) && (depth input) <= maxi
     then op input
     else input

sharePrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
sharePrefix l1 l2 =
  let
    prefix = Prelude.map fst $ takeWhile (uncurry (==)) $ zip l1 l2
    f = drop $ length prefix
  in
  (prefix, f l1, f l2)



lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs


cleanup :: AST.Expr -> AST.Expr
--cleanup (AST.Res (AST.NDSplit []) innerExpr) = innerExpr
cleanup (AST.Res (AST.NDMerge mFctrs) (AST.Res (AST.NDSplit sFctrs) input)) =
  let (prefix, mSuffix, sSuffix) = sharePrefix mFctrs sFctrs
      in
        if (mSuffix /= [] && sSuffix /= [])
          then (AST.Res (AST.NDMerge mSuffix) (AST.Res (AST.NDSplit sSuffix) input))
          else (AST.Res (AST.NDDistr mFctrs sFctrs) input)

cleanup (AST.Res (AST.NDSplit sFctrs) (AST.Res (AST.NDMerge mFctrs) input)) =
  AST.Res (AST.NDDistr sFctrs mFctrs) input

cleanup node@(AST.Res (AST.NDDistr sFctrs mFctrs ) input) =
    if (sFctrs == mFctrs)
      then input
      else node


cleanup e = e




applyTransformChain :: [(AST.Expr -> AST.Expr)] -> AST.Assignment -> AST.Assignment
applyTransformChain transforms (AST.Assign lhs rhs ) =
  AST.Assign lhs ( trChain rhs)
    where
      trChain = Data.List.foldr (.) id $ Prelude.map transform transforms



canonicalChain = applyTransformChain    [cleanup, inlineLets.  inlineLets]
transformChain = applyTransformChain    [cleanup, splitInputsBy 2, inlineLets, inlineLets]
transformChain2 = applyTransformChain   [fz . depthOp 1 4 (splitInputsBy 8), splitInputsBy 2, inlineLets]
transformChainClean = applyTransformChain   [cleanup , fz , inlineLets]
