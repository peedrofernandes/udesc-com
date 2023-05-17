module ParserProps where

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as T

type Id = String
data Tipo = TDouble | TInt | TString | TVoid deriving Show
data TCons = CDouble Double | CInt Integer deriving Show
data Expr = Expr :+: Expr 
          | Expr :-: Expr 
          | Expr :*: Expr 
          | Expr :/: Expr
          | Neg Expr
          | Const TCons
          | IdVar Id
          | Chamada Id [Expr]
          | Lit String
          deriving Show
data ExprR = Expr :==: Expr
           | Expr :/=: Expr
           | Expr :<: Expr
           | Expr :>: Expr
           | Expr :<=: Expr
           | Expr :>=: Expr
           deriving Show
data ExprL = ExprL :&: ExprL
           | ExprL :|: ExprL
           | Not ExprL
           | Rel ExprR
           deriving Show
data Var = Id :#: Tipo deriving Show
data Funcao = Id :->: ([Var], Tipo) deriving Show
data Programa = Prog [Funcao] [(Id, [Var], Bloco)] [Var] Bloco deriving Show
-- [Funcao] = Lista de funções
-- [(Id, [Var], Bloco)] = ?
-- [Var] = Lista de variáveis globais
-- Bloco = Bloco principal

type Bloco = [Comando]
data Comando = If ExprL Bloco Bloco
             | While ExprL Bloco
             | Atrib Id Expr
             | Leitura Id
             | Imp Expr
             | Ret (Maybe Expr)
             | Proc Id [Expr]
             deriving Show

-- lingdef = emptyDef {
--   T.string = "string",
--   T.double = "double",
--   T.int = "int"
-- }

langDef = emptyDef {
  T.commentStart = "/*",
  T.commentEnd = "*/",
  T.commentLine = "//",
  T.reservedNames = ["int", "double", "string", "if", "while", "print", "read"],
  T.reservedOpNames = ["+", "-", "*", "/", "&", "|", "!", "==", "!=", ">", "<", ">=", "<="]
}

lexer = T.makeTokenParser langDef

naturalToken = T.natural lexer
symbolToken = T.symbol lexer
parensToken = T.parens lexer
bracesToken = T.braces lexer
bracketsToken = T.brackets lexer
reservedOpToken = T.reservedOp lexer
identifierToken = T.identifier lexer
reservedToken = T.reserved lexer
operatorToken = T.operator lexer
stringLiteralToken = T.stringLiteral lexer
integerToken = T.integer lexer
floatToken = T.float lexer
naturalOrFloatToken = T.naturalOrFloat lexer
whiteSpaceToken = T.whiteSpace lexer
semiToken = T.semi lexer
commaToken = T.comma lexer


type Parser u r = Parsec String u r

-- getVars :: (Tipo, [Id]) -> [Var]
-- getVars [] = []
-- getVars ((tipo, []) : t) = getVars(t)
-- getVars ((tipo, id : ids) : t) = (id :#: tipo) : getVars ((tipo, ids) : t)

getVars :: (Tipo, [Id]) -> [Var]
getVars (tipo, []) = []
getVars (tipo, id : ids) = (id :#: tipo) : getVars (tipo, ids)

extractProgramTriple :: [(Funcao, [Var], Bloco)] -> [(Id, [Var], Bloco)]
extractProgramTriple [] = []
extractProgramTriple ((id :->: t, v, b) : ts) = (id, v, b) : extractProgramTriple(ts)
