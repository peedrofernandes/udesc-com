module CompilerProps where

import Text.Parsec ( Parsec )
import Text.Parsec.Language ( emptyDef )
import qualified Text.Parsec.Token as T
import Text.Parsec.Expr (Assoc(AssocLeft, AssocRight), Operator (Prefix, Infix))
import Control.Monad.Identity (Identity)

type Id = String
data Tipo = TDouble | TInt | TString | TVoid deriving Show

instance Eq Tipo where
  TInt == TInt = True
  TDouble == TDouble = True
  TString == TString = True
  TVoid == TVoid = True
  _ == _ = False

-- instance Eq Tipo where
--   (==) :: Tipo -> Tipo -> Bool
--   (==) TInt TInt = True
--   (==) TDouble TDouble = True
--   (==) TString TString = True
--   (==) TVoid TVoid = True
--   (==) _ _ = False


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
          | IntDouble Expr
          | DoubleInt Expr
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
-- Significado do dado Programa
-- [Funcao] = Assinatura das funções

-- [(Id, [Var], Bloco)] = 
  -- Id: Nome da função
  -- [Var] = Variáveis do escopo da função
  -- Bloco = Lista de comandos que corresponde a implementação da função

-- [Var] = Variáveis usadas no bloco principal

-- Bloco = Lista de comandos que corresponde a implementação do bloco principal

type Bloco = [Comando]
data Comando = If ExprL Bloco Bloco
             | While ExprL Bloco
             | Atrib Id Expr
             | Leitura Id
             | Imp Expr
             | Ret (Maybe Expr)
             | Proc Id [Expr]
             deriving Show

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

precedenceTable :: [[Operator String u Identity Expr]]
precedenceTable = [ [unaryPrefix "-" Neg]
                  , [binaryInfix "*" (:*:) AssocLeft, binaryInfix "/" (:/:) AssocLeft]
                  , [binaryInfix "+" (:+:) AssocLeft, binaryInfix "-" (:-:) AssocLeft]
                  ]
unaryPrefix :: String -> (Expr -> Expr) -> Operator String u Identity Expr
unaryPrefix name fn = Prefix (do { reservedToken name; return fn })
binaryInfix :: String -> (Expr -> Expr -> Expr) -> Assoc -> Operator String u Identity Expr
binaryInfix name fn = Infix (do { reservedToken name; return fn })

getVars :: (Tipo, [Id]) -> [Var]
getVars (tipo, []) = []
getVars (tipo, id : ids) = (id :#: tipo) : getVars (tipo, ids)

extractProgramTriple :: [(Funcao, [Var], Bloco)] -> [(Id, [Var], Bloco)]
extractProgramTriple [] = []
extractProgramTriple ((id :->: t, v, b) : ts) = (id, v, b) : extractProgramTriple ts
