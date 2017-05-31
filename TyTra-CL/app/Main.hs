{-# OPTIONS -Wall #-}
module Main where

import qualified Cost
import           Data.String.Utils
import           DOT
import           Parser
import           ParseToAst
import           System.Environment
import           Transform
import qualified AST

pretty :: String -> String
pretty str = replace "))" "))\n" (replace "\\\"" "\"" str)


whichTransforms :: [String] -> [AST.Expr -> AST.Expr]
whichTransforms
  = map
      (\ cmd ->
         case () of
             () | cmd == "cleanup" -> cleanup
                | cmd == "inline" -> inlineLets
                | cmd == "autopar" -> fz
                | otherwise -> error "Usage: stack exec bigbird-exe <input_file> [cleanup | autopar | inline]"
      )


main :: IO ()
main = do
  args <- getArgs
  inputFile <- readFile (head args)

  let transforms   = whichTransforms $ tail args
      transformed  = applyTransformChain transforms $ trfm (run_parser parseProgram inputFile)
      in do
          putStr "Starting, graph output to: output.dot \n  \n"
          createDot transformed "./output.dot"
          putStr $ pretty $ show transformed
          putStr "\n"
          putStr $ "Cost is: " ++ (show $ Cost.computeCost transformed)
