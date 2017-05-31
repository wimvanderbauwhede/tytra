import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)


import Inference
import AST
import Cost


sampleAction :: Action
sampleAction = MOpaque (MkName "foo") [] (Prim Float) (Prim Double) defaultRawCost


sampleActionSa :: Action
sampleActionSa = MOpaque (MkName "foo") [] (Prim Float) (Prim Float) defaultRawCost



main :: IO ()
main = hspec $ do
  -- Inference Tests
  describe "infer.variable.scalar" $ do
    it "returns ty from (Var name ty)" $ do
      inferType ( Var (MkName "name") (Prim Float))
        `shouldBe` (Prim Float)

  describe "infer.variable.vector" $ do
    it "returns ty from (Var name ty)" $ do
      inferType (Var (MkName "name") (Vec (Prim Float) 32))
        `shouldBe` (Vec (Prim Float) 32)

  describe "infer.resAction.singleMap.nofactors" $ do
    it "returns Vec outTy sz for Vec inTy sz where action = map foo :: inTy -> outTy " $ do
      inferType (Res (PNDMap [] sampleAction) (Var (MkName "test") (Vec (Prim Float) 32)))
        `shouldBe` (Vec (Prim Double) 32)

  describe "infer.resAction.singleMap.factor.4" $ do
    it "returns Vec outTy sz for Vec inTy sz where action = map foo :: inTy -> outTy " $ do
      inferType (Res (PNDMap [4] sampleAction) (Var (MkName "test") (Vec (Vec (Prim Float) 8) 4 )))
        `shouldBe` (Vec (Vec (Prim Double) 8 ) 4)

  describe "infer.resAction.singleMap.factor.4.2" $ do
    it "returns Vec outTy sz for Vec inTy sz where action = map foo :: inTy -> outTy " $ do
      inferType (Res (PNDMap [2,4] sampleAction) (Var (MkName "test") (Vec (Vec (Vec (Prim Float) 32) 4 ) 2)))
        `shouldBe` (Vec (Vec (Vec (Prim Double) 32 ) 4) 2)


-- Splits and merges
  describe "infer.resNDSplit.factor.4.2" $ do
    it "returns Vec outTy sz for Vec inTy sz where action = map foo :: inTy -> outTy " $ do
      inferType (Res (NDSplit [2,4]) (Var (MkName "test") (Vec (Prim Float) 32)))
        `shouldBe` (Vec (Vec (Vec (Prim Float) 4 ) 4) 2)

  describe "infer.resNDMerge.factor.4.2" $ do
    it "returns Vec outTy sz for Vec inTy sz where action = map foo :: inTy -> outTy " $ do
      inferType (Res (NDMerge [2,4]) (Var (MkName "test") (Vec (Vec (Vec (Vec (Prim Float) 2) 4 ) 4) 2)))
        `shouldBe` (Vec (Vec (Prim Float) 2) 32)

  describe "infer.resNDMerge.nofactors" $ do
    it "returns Vec outTy sz for Vec inTy sz where action = map foo :: inTy -> outTy " $ do
      inferType (Res (NDMerge []) (Var (MkName "test") (Vec (Vec (Vec (Prim Float) 16 ) 4) 2)))
        `shouldBe` (Vec (Vec (Vec (Prim Float) 16 ) 4) 2)

  describe "infer.resNDistr.sameFactors" $ do
    it "returns Vec outTy sz for Vec inTy sz where action = map foo :: inTy -> outTy " $ do
      inferType (Res (NDDistr [2,4] [2,4]) (Var (MkName "test")  (Vec (Vec (Vec (Prim Float) 16 ) 4) 2 )))
        `shouldBe` (Vec (Vec (Vec (Prim Float) 16 ) 4) 2 )


-- distr
  describe "infer.resNDistr.sameFactorsb " $ do
    it "returns Vec outTy sz for Vec inTy sz where action = map foo :: inTy -> outTy " $ do
        inferType (Res (NDDistr [2] [2,4]) (Var (MkName "test")   (Vec (Vec (Vec (Prim Float) 16 ) 4) 2 ) ))
          `shouldBe` (Vec (Vec (Prim Float) 64 ) 2)


-- zip unzip

  describe "infer.zip.nofactors" $ do
    it "returns Vec outTy sz for Vec inTy sz where action = map foo :: inTy -> outTy " $ do
      inferType (Res (NDZipT []) (Tup [Var (MkName "name1") (Vec (Prim Float) 32), Var (MkName "name2") (Vec (Prim Double) 32)]))
      `shouldBe` (Vec (Tuple [Prim Float,Prim Double]) 32 )

  describe "infer.zip.factors.2" $ do
      it "returns Vec outTy sz for Vec inTy sz where action = map foo :: inTy -> outTy " $ do
        inferType (Res (NDZipT [2]) (Tup [Var (MkName "name1") (Vec (Prim Float) 32), Var (MkName "name2") (Vec (Prim Double) 32)]))
          `shouldBe` ( Vec (Vec (Tuple [Prim Float,Prim Double]) 16) 2 )


  describe "infer.unzip.nofactors" $ do
      it "returns Vec outTy sz for Vec inTy sz where action = map foo :: inTy -> outTy " $ do
        inferType (Res (NDUnzipT []) (Var (MkName "name1") ( Vec (Tuple [Prim Float,Prim Double]) 32 )))
          `shouldBe` (Tuple [Vec (Prim Float) 32, Vec (Prim Double) 32] )
  --
  -- describe "infer.unzip.nofactors.b" $ do
  --     it "returns Vec outTy sz for Vec inTy sz where action = map foo :: inTy -> outTy " $ do
  --       inferType (Res (NDUnzipT []) (Var (MkName "name1") ( Vec (Vec (Tuple [Prim Float,Prim Double]) 16) 2 )))
  --         `shouldBe` (Tuple [Vec (Prim Float) 32, Vec (Prim Double) 32] )
  --
  --

  describe "infer.unzip.factors.2" $ do
      it "returns Vec outTy sz for Vec inTy sz where action = map foo :: inTy -> outTy " $ do
        inferType (Res (NDUnzipT [2]) (Var (MkName "name1") ( Vec (Vec (Tuple [Prim Float,Prim Double]) 16) 2 )))
          `shouldBe` (Tuple [Vec (Prim Float) 32, Vec (Prim Double) 32] )


-- Compose
  describe "infer.compose.a-a.a-b" $ do
    it "returns Vec outTy sz for Vec inTy sz where action = map foo :: inTy -> outTy " $ do
      inferType (Res (Compose  [sampleAction,sampleActionSa]) $ (Var (MkName "name1") ( Prim Float)))
        `shouldBe` (Prim Double)
