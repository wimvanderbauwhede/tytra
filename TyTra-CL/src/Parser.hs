module Parser where


{-

import Text.Parsec hiding (State)
import Text.Parsec.Pos
--import Text.Parsec.Indent
import Control.Monad.State


-}



import           Debug.Trace
import           ParseTree
import           Text.Parsec.Indent
import qualified Text.Parsec.Token                      as P
import           Text.ParserCombinators.Parsec          hiding (runParser,Parser)
import           Text.ParserCombinators.Parsec.Language (emptyDef)

-- debug

import           Data.Functor.Identity
import           Text.Parsec                            hiding (try)
import           Text.Parsec.Prim                       hiding (try,runParser)
import qualified Data.Map                               as Map

println msg = trace (show msg) $ return ()

seeNext :: String -> Int -> ParsecT String u Identity ()
seeNext str n = do
  s <- getParserState
  let out = take n (stateInput s)
  println str
  println out
  println "--"
-- end debug

type TypeContext = Map.Map Name ([Type], Bool)
type Parser = Parsec String TypeContext



run_parser :: Parser a -> String -> a
run_parser p str = case  parseWith p str of
                     Left err -> error $ "parse error at " ++ show err
                     Right val -> val


parseWith :: Parser a -> String -> Either ParseError a
parseWith p = runParser p Map.empty "stuff happens"

hllDef    = P.LanguageDef
               { P.commentStart   = "{-}"
               , P.commentEnd     = "-}"
               , P.commentLine    = "--"
               , P.nestedComments = True
               , P.identStart     = letter <|> char '_'
               , P.identLetter    = alphaNum <|> oneOf "_'"
               , P.opStart        = P.opLetter emptyDef
               , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , P.reservedOpNames= ["::","=", "<-"]
               , P.reservedNames  = ["let","in", "zip", "unzip", "map", "fold"]
               , P.caseSensitive  = True
               }


lexer       = P.makeTokenParser hllDef --haskellDef
-- The parser

parens = P.parens lexer
brackets = P.brackets lexer
angles = P.angles lexer
braces = P.braces lexer
commaSep = P.commaSep lexer
symbol = P.symbol lexer
identifier = P.identifier lexer
integer = P.integer lexer
stringLiteral = P.stringLiteral lexer
lexeme = P.lexeme lexer
reserved    = P.reserved lexer


getTypes :: TypeContext -> Name -> [Type]
getTypes tyc name =
  let tys = Map.lookup name tyc
  in
    case tys of
      (Just tys) -> case tys of
        ([],_) -> error "nae types"
        (ttys,_) -> ttys
      Nothing -> error "Variable not bound"




parseProgram :: Parser Program
parseProgram = do {
  typeDecls <- many (try parseTypeDecl)
  ; kernel <- pLetAssignment
  ; return $ MkProgram typeDecls kernel
  }

pName :: Parser Name
pName = do {
  name <- identifier
  ; return $ MkName name
}
pFunctionOccurenceSingle :: Parser FunctionOccurence
pFunctionOccurenceSingle = do {
  name <- pName
--  ; println name
  ; return $ MkFunctionOccurence name []
  }

pFunctionOccurenceExtra :: Parser FunctionOccurence
pFunctionOccurenceExtra = do {
  name <- pName
  ; extra <- many1 pResultsInValue
  ; return $ MkFunctionOccurence name extra
  }


pFunctionMapOcc :: Parser FunctionOccurence
pFunctionMapOcc =  do {
    reserved "map"
    ; funcOcc <- pFunctionOccurenceInner
    ; return $ MkMapOccurence funcOcc
    }

pFunctionFoldOcc :: Parser FunctionOccurence
pFunctionFoldOcc = do {
    reserved "fold"
    ; funcOcc <- pFunctionOccurenceInner
    ; acc <- pResultsInValueInner
    ; return $ MkFoldOccurence funcOcc acc
    }

pFunctionLoopOcc :: Parser FunctionOccurence
pFunctionLoopOcc = do {
    reserved "loop"
    ; symbol "("
    ; start <- integer
    ; symbol ","
    ; stop <- integer
    ; symbol ","
    ; step <- integer
    ; symbol ")"
    ; stmnt <- pFunctionOccurenceInner
    ; return $ MkLoopOccurence start stop step stmnt
    }

pFunctionComposeOcc :: Parser FunctionOccurence
pFunctionComposeOcc =  do {
  f1 <- pFunctionOccurenceInner
  ; symbol "."
  ; functions <- sepBy1 pFunctionOccurenceInner (symbol ".")
  ; return $ MkCompose (f1:functions)
  }

-- sepBy1 is not enough to guarantee that a . for compose will appear
pFunctionOccurence :: Parser FunctionOccurence
pFunctionOccurence =  pFunctionMapOcc
                      <|> pFunctionFoldOcc
                      <|> pFunctionLoopOcc
                      <|> try (parens pFunctionComposeOcc) --
                      <|> parens pFunctionOccurenceExtra
                      <|> pFunctionOccurenceSingle
                      <|> fail "my-failure"

pFunctionOccurenceInner :: Parser FunctionOccurence
pFunctionOccurenceInner = parens (  pFunctionMapOcc
                                    <|> pFunctionFoldOcc
                                    <|> pFunctionLoopOcc
                                    <|> try pFunctionComposeOcc --
                                    <|> pFunctionOccurenceExtra
                                  )
                          <|> pFunctionOccurenceSingle
                          <|> fail "my-failure"

-- map foo input
-- map (foo extra) input
-- map foo foo2 input
pFunctionApplication :: Parser ResultsInValue
pFunctionApplication = do {
  funcOcc <- pFunctionOccurence
  ; input <- pResultsInValueInner
  ; return $ MkFunctionApplication funcOcc input
  }

pLetLhsSingle :: Parser LetLhs
pLetLhsSingle = do {
  name <- pName;
  ; return $ MkLetLhs [name]
  }

pLetLhsMult :: Parser LetLhs
pLetLhsMult = do {
  symbol "("
  ; names <- sepBy1 pName (symbol ",")
  ; symbol ")"
  ; return $ MkLetLhs names
  }

pLetLhs :: Parser LetLhs
pLetLhs = pLetLhsMult
  <|> pLetLhsSingle

pLetAssignment :: Parser LetAssignment
pLetAssignment = do {
  letLhs <- pLetLhs
  ; reserved "="
  ; letRhs <- pResultsInValue
  ; return $ MkLetAssignment  letLhs letRhs
  }

pLet :: Parser ResultsInValue
pLet = do {
  reserved "let"
  ; assgns <- many1 pLetAssignment
  ; reserved "in"
  ; inStmnt <- pResultsInValue
  ; return $ MkLet assgns inStmnt
  }

pZip :: Parser ResultsInValue
pZip = do {
  reserved "zip"
  ; names <- parens (pResultsInValue `sepBy1` symbol ",")
  ; return $ MkZip names
  }

pUnzip :: Parser ResultsInValue
pUnzip = do {
  reserved "unzip"
  ; name <- pResultsInValueInner
  ; return $ MkUnzip name
  }

pResultsInValue :: Parser ResultsInValue
pResultsInValue = pLet
                  <|> pZip
                  <|> pUnzip
                  <|> try pFunctionApplication
                  <|> pVariableOccurence

pResultsInValueInner :: Parser ResultsInValue
pResultsInValueInner = parens ( pLet
                                <|> pZip
                                <|> pUnzip
                                <|> pFunctionApplication
                              )
                      <|> pVariableOccurence


pVariableOccurence :: Parser ResultsInValue
pVariableOccurence = do {
  name <- pName;
  ; return $ MkVariable name
  }


{-

Type declartions

Example: someType :: Type -> Type -> Type
-}
parseTypeDecl :: Parser TypeDecl
parseTypeDecl = do {
  name <- pName
  ; assoc <- option False ( symbol "assoc" >> return True)
  ; reserved "::"
  ; tys <- sepBy1 parseType (symbol "->")
  ; modifyState (Map.insert name (tys, assoc))
  ; return $ MkTypeDecl name tys assoc
  }

parseType :: Parser Type
parseType = parseVectorType
  <|> parseTupleType
  <|> parsePrimType


parseVectorType :: Parser Type
parseVectorType = do {
  symbol "Vec"
  ; size <- integer
  ; ty <- parseType
  ; return $ Vec ty size
  }

parseTupleType :: Parser Type
parseTupleType = do {
  tys <- parens (sepBy parseType (symbol ","))
  ; return $ Tuple tys
  }

floatP, doubleP, shortP, charP, fixP :: Parser PrimType
floatP = symbol "Float" >> return Float
doubleP = symbol "Double" >> return Double
shortP = symbol "Short" >> return Short
charP =  symbol "Char" >> return Char
fixP = do
  symbol "Fix"
  int <- integer
  char '.'
  rem <- integer
  return $ Fix int rem


primTypeP :: Parser PrimType
primTypeP = try floatP
  <|> doubleP
  <|> shortP
  <|> charP
  <|> fixP

parsePrimType :: Parser Type
parsePrimType = do {
  ty <- primTypeP
  ; return $ Prim ty
  }
