module Cost where

import           AST
import           Data.Generics.Uniplate.Data
import           Inference
import qualified Debug.Trace(trace)

defaultRawCost = 1



computePrimCost :: PrimType -> RawCost
computePrimCost pty =
  case pty of
    Float -> defaultRawCost { size =  MkResourceUse { luts = 32, blockRams = 32, multipliers = 0 } }
    Double -> defaultRawCost { size =  MkResourceUse { luts = 64, blockRams = 64, multipliers = 0 }}
    Short -> defaultRawCost { size =  MkResourceUse { luts = 64, blockRams = 64, multipliers = 0 } }
    Char -> defaultRawCost { size =  MkResourceUse { luts = 8, blockRams = 8, multipliers = 0 } }
    Fix intg remain -> defaultRawCost { size =  MkResourceUse { luts = intg, blockRams = intg, multipliers = 0 }
                                                +  MkResourceUse { luts = remain, blockRams = remain, multipliers = 0 } }

computeTypeCost :: Type -> RawCost
computeTypeCost ty =
  case ty of
    Prim pty -> computePrimCost pty
    Vec innerty sz -> computeTypeCost innerty * fromInteger sz
    Tuple tys -> sum (map computeTypeCost tys)



appendCost :: RawCost -> RawCost -> RawCost
appendCost  MkRawCost { delay = d1, latency = l1, size = s1}
            MkRawCost { delay = d2, latency = l2, size = s2}
            =
              MkRawCost { delay = d1 + d2, latency = l1 + l2, size = s1}


maxLatencySumResource :: RawCost -> RawCost -> RawCost
maxLatencySumResource   MkRawCost { delay = d1, latency = l1, size = s1}
                        MkRawCost { delay = d2, latency = l2, size = s2}
                        =
                          MkRawCost { delay = max d1 d2 , latency =  max l1 l2, size = s1 + s2}

{-


  t0 + TD = one item of work

  RawCOst

-}
computeExprCost :: Expr -> RawCost
computeExprCost expr =
  case expr of
    Var _ ty -> computeTypeCost ty
    Res act input ->
      let
        inputCost = computeExprCost input
        MkRawCost { delay = id1, latency = il1, size = is1} = inputCost
        outputCost = computeTypeCost (inferType expr)
        inputType = inferType input
        inputSize = sizeTy inputType
      in
        case act of
          MOpaque _ extra _ _ rc -> rc
            -- latency -> computation can start once the inputCost's latency + delay have been met
            --
            -- let
            --   extraCost = foldl1 maxLatencySumResource $ map computeExprCost extra
            --   inputExtraCost = maxLatencySumResource inputCost extraCost
            --   in
            --     Debug.Trace.trace ("cost of extra: " ++ show extraCost) $ rc + inputCost + (extraCost)  + outputCost
          FOpaque assoc _ extra acc _ _ rc -> foldl1 maxLatencySumResource $ (map computeExprCost extra) ++  [(computeExprCost acc)] ++ [rc] -- + inputCost
          PNDMap fctrs iact ->
            case (fctrs,inputType) of
              ([],Vec innerTy sz) ->
                let MkRawCost { delay = d1, latency = l1, size = s1} = computeExprCost (Res iact (Var (MkName "foo") innerTy))
                    in
                      MkRawCost { delay = d1 * inputSize, latency = il1 + l1 , size = is1 + s1}
              (x:xs,Vec innerTy sz) ->
                if (x == sz)
                  then
                    let MkRawCost { delay = d1, latency = l1, size = s1} = computeExprCost (Res (PNDMap xs iact) (Var (MkName "foo") innerTy))
                        in
                          MkRawCost { delay = d1 * x, latency = il1 + l1 , size = is1 + (s1 * fromInteger x )}  -- FIXME: this accounts for input size at every factor, should be once
                  else
                    error "mismatch in map"
              _ -> error "expecting vector"
                    --iact * fromInteger (sizeTy $ inferType input) + inputCost
          PNDFold fctrs iact ->
            if isAssoc iact
              then
                case (fctrs,inputType) of
                  ([],Vec innerTy sz) ->
                    let MkRawCost { delay = d1, latency = l1, size = s1} = computeExprCost (Res iact (Var (MkName "foo") innerTy))
                      in
                        MkRawCost { delay = d1 * inputSize, latency = il1 + l1 , size = is1 + s1}
                  (x:xs,Vec innerTy sz) ->
                    if (x == sz)
                      then
                        let MkRawCost { delay = d1, latency = l1, size = s1} = computeExprCost (Res (PNDFold xs iact) (Var (MkName "foo") innerTy))
                            in
                              MkRawCost { delay = d1 * x, latency = il1 + l1 , size = is1 + (s1 * fromInteger x)} -- FIXME: this accounts for input size at every factor, should be once
                      else
                        error "mismatch in fold"
                  _ -> error "expecting vector"
              else
                case inputType of
                  Vec innerTy sz ->
                    let MkRawCost { delay = d1, latency = l1, size = s1} = computeExprCost (Res iact (Var (MkName "foo") innerTy))
                        in
                          MkRawCost { delay = d1 * inputSize, latency = il1 + l1 , size = is1 + s1}
                  _ -> error "expecting vector"

          NDMap vfctrs var iact ->
            case (vfctrs,inputType) of
                ([],Vec innerTy sz) ->
                  let MkRawCost { delay = d1, latency = l1, size = s1} = computeExprCost (Res iact (Var (MkName "foo") innerTy))
                    in
                      case var of
                        Par ->   MkRawCost { delay = d1 , latency = il1 + l1 , size = is1 + (s1 * fromInteger sz)}
                        Pipe -> MkRawCost { delay = d1 * inputSize, latency = il1 + l1 , size = is1 + s1}
                        Seq ->  MkRawCost { delay = d1 * inputSize, latency = il1 + l1 , size = is1 + s1}

                (x:xs,Vec innerTy sz) ->
                  if (fst x == sz)
                    then
                      let MkRawCost { delay = d1, latency = l1, size = s1} = computeExprCost (Res (NDMap xs var iact) (Var (MkName "foo") innerTy))
                          in
                            case snd x of
                              Par ->  MkRawCost { delay = d1 * fst x, latency = il1 + l1 , size = is1 + (s1 * fromInteger (fst x))} -- FIXME: this accounts for input size at every factor, should be once
                              Pipe -> MkRawCost { delay = d1 * fst x, latency = il1 + l1 , size = is1 + (s1 * fromInteger (fst x))} -- FIXME: this accounts for input size at every factor, should be once
                              Seq ->  MkRawCost { delay = d1 * fst x, latency = il1 + l1 , size = is1 + (s1 * fromInteger (fst x))} -- FIXME: this accounts for input size at every factor, should be once
                    else
                      error "mismatch in fold"
                _ -> error "expecting vector"

          NDFold vfctrs var iact ->
            if isAssoc iact
              then
                case (vfctrs,inputType) of
                  ([],Vec innerTy sz) ->
                    let MkRawCost { delay = d1, latency = l1, size = s1} = computeExprCost (Res iact (Var (MkName "foo") innerTy))
                      in
                        case var of
                          Tree -> MkRawCost { delay = d1 * inputSize, latency = il1 + l1 , size = is1 + s1}
                          FPipe -> MkRawCost { delay = d1 * inputSize, latency = il1 + l1 , size = is1 + s1}
                          FSeq -> MkRawCost { delay = d1 * inputSize, latency = il1 + l1 , size = is1 + s1}


                  (x:xs,Vec innerTy sz) ->
                    if (fst x == sz)
                      then
                        let MkRawCost { delay = d1, latency = l1, size = s1} = computeExprCost (Res (NDFold xs var iact) (Var (MkName "foo") innerTy))
                            in
                              case snd x of
                                Tree -> MkRawCost { delay = d1 * fst x, latency = il1 + l1 , size = is1 + (s1 * fromInteger (fst x))} -- FIXME: this accounts for input size at every factor, should be once
                                FPipe -> MkRawCost { delay = d1 * fst x, latency = il1 + l1 , size = is1 + (s1 * fromInteger ( fst x))} -- FIXME: this accounts for input size at every factor, should be once
                                FSeq -> MkRawCost { delay = d1 * fst x, latency = il1 + l1 , size = is1 + (s1 * fromInteger ( fst x))} -- FIXME: this accounts for input size at every factor, should be once

                      else
                        error "mismatch in fold"
                  _ -> error "expecting vector"
              else
                case inputType of
                  Vec innerTy sz ->
                    let MkRawCost { delay = d1, latency = l1, size = s1} = computeExprCost (Res iact (Var (MkName "foo") innerTy))
                        in
                          MkRawCost { delay = d1 * inputSize, latency = il1 + l1 , size = is1 + s1}
                  _ -> error "expecting vector"
          NDSplit _ -> defaultRawCost + 2 + inputCost
          NDMerge _ -> defaultRawCost + inputCost
          NDDistr _ _ -> defaultRawCost + inputCost
          NDZipT _ -> computeTypeCost (inferType input) + inputCost
          NDUnzipT _ -> computeTypeCost (inferType input)+ inputCost
          Compose acts -> (sum $ map computeActionCost acts) + inputCost
          Let lhs rhs -> computeExprCost rhs + inputCost
          Loop start stop step  act ->
            let repeatFactor =  ( (stop - start) `div` step )
                in
                appendCost (computeActionCost act * fromInteger repeatFactor) inputCost


    Tup xs -> sum (map computeExprCost xs)


-- computeFactoredCost :: [Integer] -> Action -> RawCost
-- computeFactoredCost factors iact = case factors of
--   [] -> computeActionCost iact * fromInteger ( sizeTy $ inferTypeOfAction iact )
--   _  -> product $ map ((computeActionCost iact *) . fromInteger)  factors

computeActionCost :: Action -> RawCost
computeActionCost act = defaultRawCost
  -- case act of
  -- MOpaque _ _ _ _ rc -> rc
  -- FOpaque _ _ _ _ _ _ rc -> rc
  -- PNDMap factors iact -> computeFactoredCost factors iact
  -- PNDFold factors iact ->
  --   case isAssoc iact of
  --     True -> computeFactoredCost factors iact
  --             -- let
  --             --     MkRawCost { delay = d1, latency = l1, size = s1} = computeFactoredCost factors iact
  --             --     n = case factors of
  --             --       [] -> sizeTy $ inferType iact
  --             --       _ -> product factors
  --             -- in
  --             --     MkRawCost { }
  --     False -> computeFactoredCost factors iact
  -- NDMap _ _ iact ->  computeActionCost iact
  -- NDFold _ iact ->  computeActionCost iact
  -- NDSplit _ -> defaultRawCost + 2
  -- NDMerge _ -> defaultRawCost
  -- NDDistr _ _ -> defaultRawCost
  -- NDZipT _ ty -> computeTypeCost ty
  -- NDUnzipT _ ty -> computeTypeCost ty
  -- Compose acts -> sum $ map computeActionCost acts
  -- Let lhs rhs -> computeExprCost rhs
  -- Loop start stop step  act -> computeActionCost act -- * ( (stop - start) / step )


computeCost :: Assignment -> RawCost
computeCost (Assign lhs rhs) = computeExprCost rhs + computeExprCost lhs





{- Propagation Cost
   How many cycles does it take to buffer
-}
