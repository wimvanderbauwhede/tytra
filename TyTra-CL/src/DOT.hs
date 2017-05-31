{-# LANGUAGE OverloadedStrings #-}

module DOT where

import qualified AST
import qualified Cost
import           Data.Graph.Inductive
import           Data.Graph.Inductive.Dot
import           Inference
import qualified Transform
--import Data.Text.Lazy
import           Control.Monad            (forM, liftM)
import           Control.Monad.State
import           Text.Dot

type NodeManagement = State Node
genNodeId :: NodeManagement Node
genNodeId = do
    n <- get
    put (n+1)
    return n

createNode :: String -> NodeManagement (LNode String)
createNode label = do
  iid <- genNodeId
  return (iid, label)

createEdge :: LNode String -> LNode String -> String -> NodeManagement (LEdge String)
createEdge (x,_) (y,_) label = return (x,y,label)
-- mkGraph :: [LNode a] -> [LEdge b] -> gr a

maxNode :: Gr a b -> Node
maxNode gr = case nodeRange gr  of
              (_,max) -> max


-- type Context a b = (Adj b, Node, a, Adj b)
-- type Adj b = [(b, Node)]

--exprtoCont (AST.Var name ty) maxNode =
--exprToCont :: AST.Expr -> Node ->  Context String String


-- expand :: AST.Expr -> Node-> NodeManagement (Context String String)
-- expand (AST.Var name ty) outerNode =



class ToGraph a where
  toGr :: a -> NodeManagement (Gr String String)

actionToGraph :: AST.Action -> AST.Expr -> NodeManagement (Gr String String)
actionToGraph action input =
  case action of
    (AST.MOpaque (AST.MkName name) extra _ _ _ ) ->
      do
        n <- get
        nd <- createNode $ "func " ++ name
        extraGr <- mapM toGr extra
        let
          eNodesLst = mapM labNodes extraGr
          heads = map head eNodesLst
          eNodes = Prelude.concat eNodesLst
          eEdges = Prelude.concat $ mapM labEdges extraGr
          in
            do
             edgs <- mapM ( \(x,y) -> createEdge x nd (show $ inferType y)) $ zip heads extra
             return $ mkGraph (nd:eNodes) $ eEdges++edgs

    (AST.FOpaque _ (AST.MkName name) _ acc  _ _ _ ) ->
      do
        nd <- createNode name
        acn <- toGr acc
        let acns = labNodes acn
            acne = labEdges acn
            in
              do
                edg <- createEdge (head acns) nd (show $ inferType acc)
                return $ mkGraph (nd:acns) (edg:acne)

    (AST.Compose acts) ->
      case acts of
        [] -> do
          fake <- createNode "end"
          return $ mkGraph [] []
        x:xs -> do
          nd <- actionToGraph x input
          rstGraph <- actionToGraph (AST.Compose xs) input
          let ndNodes = labNodes nd
              rstNodes = labNodes rstGraph
            in
              if (xs /= [])
               then do
                  edg <-  createEdge (head rstNodes) (head ndNodes) "" --(show $ (typeOfAction action) $ inferType input)
                  return $ mkGraph (ndNodes ++ rstNodes) $ edg:(labEdges rstGraph ++ labEdges nd)
               else
                  return $ mkGraph (ndNodes ++ rstNodes) $  labEdges nd

    (AST.PNDMap _ act) ->
      do
        nd <- createNode "map"
        actGraph <- actionToGraph act input
        let
          actNodes = labNodes actGraph
          in
            do
              edg <- createEdge (head actNodes) nd "" -- (show (typeOfAction action (inferType input)))
              return $ mkGraph (actNodes++[nd]) $ edg:labEdges actGraph
    (AST.NDMap fctrs var act) ->
        do
          nd <- createNode $ "map " ++ show fctrs ++ " " ++ show var
          actGraph <- actionToGraph act input
          let
            actNodes = labNodes actGraph
            in
              do
                edg <- createEdge (head actNodes) nd "" -- (show (typeOfAction action (inferType input)))
                return $ mkGraph (actNodes++[nd]) $ edg:labEdges actGraph
    (AST.PNDFold fctrs act) ->
      do
        nd <- createNode $ "fold " ++ show fctrs
        actGraph <- actionToGraph act input
        let
          actNodes = labNodes actGraph
          in
            do
              edg <- createEdge (head actNodes) nd (show $ (typeOfAction action) $ inferType input)
              return $ mkGraph (actNodes++[nd]) $ edg:labEdges actGraph

    (AST.NDFold fctrs var act) ->
        do
          nd <- createNode $ "fold" ++ show fctrs ++ " " ++ show var
          actGraph <- actionToGraph act input
          let
            actNodes = labNodes actGraph
            in
              do
                edg <- createEdge (head actNodes) nd (show $ (typeOfAction action) $ inferType input)
                return $ mkGraph (actNodes++[nd]) $ edg:labEdges actGraph
    (AST.NDSplit ints) ->
      do
        nd <- createNode $ "split" ++ show ints
        return $ mkGraph [nd] []
    (AST.NDDistr sFctrs mFctrs) ->
      do
        nd <- createNode $ "distr" ++ show sFctrs ++ " " ++ show mFctrs
        return $ mkGraph [nd] []

    (AST.NDMerge ints) ->
      do
        nd <- createNode $ "merge" ++ show ints
        return $ mkGraph [nd] []

    (AST.NDZipT ints) ->
      do
         -- nd <- createNode $ "zip" ++ show ints
        inGraph <- toGr input
        let
          inodes = labNodes inGraph
          redges = labEdges inGraph
          in
            do
              -- edg <- createEdge (head inodes) nd (show $ inferType input)
              return $ mkGraph (inodes) (redges)

    (AST.NDUnzipT ints) ->
      do
        nd <- createNode $ "unzip" ++ show ints
        inGraph <- toGr input
        let
          inodes = labNodes inGraph
          redges = labEdges inGraph
          in
            do
              edg <- createEdge nd (head inodes)  (show $ inferType input)
              return $ mkGraph (nd:inodes) (edg:redges)

    (AST.Let lhs rhs) ->
      do
        lhsGraph <- toGr lhs
        rhsGraph <- toGr rhs
        let lnodes = labNodes lhsGraph
            rnodes = labNodes rhsGraph
            in
              do
                edg <- createEdge (head rnodes) (head lnodes) (show $ inferType rhs)
                return $ mkGraph (lnodes ++ rnodes) $ [edg] ++ labEdges lhsGraph ++ labEdges rhsGraph

    (AST.Loop start stop step act ) ->
      do
        nd <- createNode $ "loop start:" ++ show start ++ "stop: " ++ show stop ++ " step: " ++ show step
        actGraph <- actionToGraph act input
        let
          actNodes = labNodes actGraph
          in
            do
              edg <- createEdge nd (head actNodes)  (show $ (typeOfAction action) $ inferType input)
              return $ mkGraph (actNodes++[nd]) $ edg:labEdges actGraph

    -- _ ->
    --    do
    --     n <- get
    --     nd <- createNode $ "action" ++ show n
    --     return $ mkGraph [nd] []

instance ToGraph AST.Expr where
  toGr node@(AST.Var (AST.MkName name) ty) = do
    nd <- createNode $ name
    return $ mkGraph [nd] []

  toGr node@(AST.Res act expr) = do
    nd <- actionToGraph act expr
    actGraph <- actionToGraph act expr
    exprGraph <- toGr expr
    let exprNodes = labNodes exprGraph
        actNodes = labNodes actGraph
      in
        do
          edg <- createEdge (head exprNodes) (head actNodes) (show (inferType expr) ++ (show $ Cost.computeExprCost expr)  ++ "depth: " ++  (show $ Transform.depth expr) )
          return $ mkGraph (actNodes ++ exprNodes) $ edg:(labEdges exprGraph ++ labEdges actGraph)

  toGr node@(AST.Tup exprs) = do
      exprGraph <- mapM toGr exprs
      nd <- createNode $ "tuple of"
      let exprNodesLst = map labNodes exprGraph
          exprEdgesLst = map labEdges exprGraph
          in do
            edgs <- mapM ( \z -> createEdge z nd "asdf") (map head exprNodesLst)   --(show $ inferType expr)
            return $ mkGraph (nd:(concat exprNodesLst)) $ edgs ++ (concat exprEdgesLst)

  -- toGr _ = do
  --     lhsNode <- createNode "Output"
  --     rhsNode <- createNode "Empty"
  --     edge <- createEdge rhsNode lhsNode "Str"
  --     return $ mkGraph [lhsNode,rhsNode] [edge]


instance ToGraph AST.Assignment where
  toGr (AST.Assign lhs rhs) = do
    lhsGraph <- toGr lhs
    rhsGraph <- toGr rhs
    let lnodes = labNodes lhsGraph
        rnodes = labNodes rhsGraph
        in
          do
            edg <- createEdge (head rnodes) (head lnodes) (show $ inferType rhs)
            return $ mkGraph (lnodes ++ rnodes) $ [edg] ++ labEdges lhsGraph ++ labEdges rhsGraph





createDot :: AST.TyTraHLLProgram -> String ->  IO ()
createDot ast str =
  let
    graph = evalState (toGr ast) 0
    dot = showDot (fglToDot graph)
  in
    do
      writeFile str dot
      print $ show $ maxNode graph
