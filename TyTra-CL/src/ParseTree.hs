{-# LANGUAGE DeriveDataTypeable #-}

module ParseTree where

import           Data.Data
import           Data.Generics
import           Data.Typeable

data Name = MkName String deriving (Data, Typeable, Show, Eq, Ord)

data Program = MkProgram [TypeDecl] LetAssignment
  deriving (Data, Typeable, Show, Eq)

data TypeDecl = MkTypeDecl Name [Type] Bool
  deriving (Data, Typeable, Show, Eq)

data Type =
  Vec Type Integer
  | Tuple [Type]
  | Prim PrimType
    deriving (Data, Typeable, Show, Eq)

data PrimType = Float
                | Double
                | Short
                | Char
                | Fix Integer Integer
                deriving (Data, Typeable, Show, Eq)

data FunctionOccurence =
  MkFunctionOccurence Name [ResultsInValue]
  | MkMapOccurence FunctionOccurence
  | MkFoldOccurence FunctionOccurence ResultsInValue
  | MkLoopOccurence Integer Integer Integer FunctionOccurence
  | MkCompose [FunctionOccurence]
  deriving (Data, Typeable, Show, Eq)

data LetLhs = MkLetLhs [Name]
  deriving (Data, Typeable, Show, Eq)

data LetAssignment = MkLetAssignment LetLhs ResultsInValue
  deriving (Data, Typeable, Show, Eq)

data ResultsInValue =
  MkVariable Name
  | MkFunctionApplication FunctionOccurence ResultsInValue
  | MkLet [LetAssignment] ResultsInValue
  | MkZip [ResultsInValue]
  | MkUnzip ResultsInValue
  deriving (Data, Typeable, Show, Eq)
