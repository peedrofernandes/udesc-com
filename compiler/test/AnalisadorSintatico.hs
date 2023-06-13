-- Trabalho realizado por Fernando Martins e Luigi Boscatto -------------------------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Redundant bracket" #-}

import Data.Maybe
import Data.Void (vacuous)
import GHC.Read (paren)
import Gramar
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token (GenTokenParser (reserved))
import Text.Parsec.Token qualified as T
import Text.XHtml (base)
import Data.List (unzip4)

type Parser u r = Parsec String u r

lingDef =
  emptyDef
    { T.commentStart = "{-",
      T.commentEnd = "-}",
      T.commentLine = "--",
      T.reservedOpNames =
        [ "+",
          "-",
          "/",
          "*",
          "==",
          ">=",
          "<=",
          ">",
          "<",
          "!=",
          "&&",
          "||",
          "!",
          "="
        ],
      T.reservedNames =
        [ "int",
          "double",
          "string",
          "void",
          "if",
          "while",
          "print",
          "read",
          "else"
        ]
    }

lexico = T.makeTokenParser lingDef

int = T.integer lexico

double = T.float lexico

texto = T.identifier lexico

symbol = T.symbol lexico
litStr = T.stringLiteral lexico
parens = T.parens lexico

comma = T.comma lexico

semicolon = T.semi lexico

braces = T.braces lexico

reservedOp = T.reservedOp lexico

reservedName = T.reserved lexico

-- expr ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

tabela = [ 
    [prefix "-" Neg],
    [binario "*" (:*:) AssocLeft, binario "/" (:/:) AssocLeft],
    [binario "+" (:+:) AssocLeft, binario "-" (:-:) AssocLeft]
  ]

binario name fun assoc = Infix (do reservedOp name; return fun) assoc

prefix name fun = Prefix (do reservedOp name; return fun)

expr =
  buildExpressionParser tabela fator
    <?> "expression"

cons =
  do n <- int; return (Const (CInt n))
    <|> do d <- double; return (Const (CDouble d))
    <|> do id <- texto; return (IdVar id)
    <|> do s <- litStr; return (Lit s)
    <|> do id <- texto; e <- many expr; return (Chamada id e)

fator =
  parens expr
    <|> do c <- cons; return c
    <?> "simple expression"

-- exprR ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

opR =
  do reservedOp "=="; return (:==:)
    <|> do reservedOp ">="; return (:>=:)
    <|> do reservedOp "<="; return (:<=:)
    <|> do reservedOp ">"; return (:>:)
    <|> do reservedOp "<"; return (:<:)
    <|> do reservedOp "!="; return (:/=:)

exprR =
  parens exprR
    <|> do n1 <- expr; o <- opR; n2 <- expr; return (o n1 n2)

-- exprL ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

exprL = buildExpressionParser tabelaL fatorL

tabelaL =
  [ [prefixL "!" Not],
    [binarioL "&&" (:&:) AssocLeft],
    [binarioL "||" (:|:) AssocLeft]
  ]

binarioL name fun assoc = Infix (do reservedOp name; return fun) assoc

prefixL name fun = Prefix (do reservedOp name; return fun)

fatorL =
  parens exprL
    <|> do c <- exprR; return (Rel c)

-- declarações ----------------------------------------------------------------------------------------------------------------------------------------------------------------

declarVac = do
  t <- tipo
  lv <- many1 singleID
  semicolon
  let r = map (cTipo t) lv
  return r

tipo =
  do reservedName "int"; return (TInt)
    <|> do reservedName "double"; return (TDouble)
    <|> do reservedName "string"; return (TString)
    <|> do reservedName "void"; return (TVoid)

singleID =
  do s <- texto; return s
    <|> do comma; s <- texto; return s

cTipo t id = (id :#: t)

-- funcoes --------------------------------------------------------------------------------------------------------------------------------------------------------------------

funcao :: Parser u (Funcao, Id)
funcao = 
  do
    t <- tipo
    id <- texto
    v <- parens (many argFunc)
    return (id :->: (v, t), id)

argFunc = 
  do
    t <- tipo
    id <- texto
    return (id :#: t)
  <|> do 
        comma
        t <- tipo
        id <- texto
        return (id :#: t)

-- comandos -------------------------------------------------------------------------------------------------------------------------------------------------------------------

bloco =
  do
    listaCmd <- braces (many comando)
    return (listaCmd) -- PERGUNTAR PRO PEDRO SOBRE [] quando usamos retorno [listaCmd]
    -- deu erro no While e b

comando =
  do try atrib
    <|> do try printExpr
    <|> do try mread
    <|> do try callFunc
    <|> do try mif
    <|> do try mreturn
    <|> do try while

mreturn =
  try (do
    reservedName "return"
    e <- expr
    semicolon
    return (Ret (Just e)))
  <|> do
        reservedName "return"
        semicolon
        return (Ret Nothing)

mif =
  do
    reservedName "if"
    e <- parens exprL
    b <- bloco
    s <- senao
    return (If e b s) 

senao =
  do
    reservedName "else"
    b <- bloco
    return b
  <|>
    do
      return []

while =
  do
    reservedName "while"
    e <- parens exprL
    b <- bloco
    return (While e b)

atrib =
  do
    id <- texto
    reservedOp "="
    e <- expr
    semicolon
    return (Atrib id e)

printExpr =
  do
    reservedName "print"
    e <- parens expr
    semicolon
    return (Imp e)

mread =
  do
    reservedName "read"
    id <- parens texto
    semicolon
    return (Leitura id)

callFunc =
  do
    id <- texto
    p <- parens (many parametro)
    semicolon
    return (Proc id p)

parametro =
  do e <- expr; return e
    <|> do comma; e <- expr; return e

-- programa -------------------------------------------------------------------------------------------------------------------------------------------------------------------

mfst (a,b,c) = a
msnd (a,b,c) = b 
mtrd (a,b,c) = c 

mList [] _ _ = []
mList (x:xs) (y:ys)(z:zs) = (x,y,z) : mList xs ys zs

-- programa :: Parser u Programa -- Prog [Funcao] [(id, [Var], Bloco)] [Var] Bloco
-- programa = 
--   do 
--     -- aux <- many blocFunc -- [(Funcao, String, ([Var], [Comando]))]
--     -- let f = map mfst aux -- [Funcao]
--     -- let id = map msnd aux -- [Id]
--     -- let tird = map mtrd aux -- []
--     -- let var = map fst tird
--     -- let comando = map snd tird
--     -- (f, id, var, comando) <- unzip (many blocFunc)
--     -- [(f, id, (var, comando))] <- many blocFunc
--     -- (f, id, var, bloco) <- unzip4 (many blocFunc)
--     list <- many blocFun
--     let cf = mList id var comando
--     mb <- braces mainBlock
--     let var2 = fst mb
--     let b = snd mb
--     return (Prog f cf var2 b)
programa :: Parser u Programa -- Prog [Funcao] [(Id, [Var], Bloco)] [Var] Bloco
programa = do { list <- many blocFunc -- [(Funcao, Id, [Var], Bloco)]
              ; let (f, id, var, block) = unzip4 list
              ; return $ Prog f cf var2 b }

blocFunc :: Parser u (Funcao, Id, [Var], Bloco)
blocFunc = 
  do
    (f, id) <- funcao
    (varList, block) <- braces (mainBlock)
    return (f, id, varList, block)  

mainBlock =
  do
    v <- many declarVac
    let retorno = concat v
    cmds <- many comando
    return (retorno, cmds)

-- main -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

partida =
    do
      programa >>= \ e -> return e
parserE e = runParser partida [] "Expressoes" e

parserExpr s = case parserE s of
  Left er -> print er
  Right v -> (print v)

main =
  do
    e <- readFile "teste.txt"
    parserExpr e
