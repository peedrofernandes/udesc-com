{-
Objetivo: Criar um Analisador Sintático Preditivo, com a seguinte gramática:

<Programa>          -> <ListaFuncoes> <BlocoPrincipal>
<ListaFuncoes>      -> <Funcao> <ListaFuncoes>
                    | vazio
<Funcao>            -> <TipoRetorno> id (<DeclParametros>) <BlocoPrincipal>
<TipoRetorno>       -> <Tipo>
                    | void
<DeclParametros>    -> <Tipo> id <Parametro>
                    | void
<Parametros>        -> , <DeclParametros>
                    | vazio
<BlocoPrincipal>    -> {<BlocoPrincipal'>}
<BlocoPrincipal'>   -> <Declaracoes> <ListaCmd>
<Declaracoes>       -> <Tipo> <ListaId>; <Declaracoes>
                    | vazio
<Tipo>              -> int
                    | string
                    | double
<ListaId>           -> id <ListaId'>
<ListaId'>          -> , <ListaId>
                    | vazio
<Bloco>             -> {<ListaCmd>}
<ListaCmd>          -> <Comando> <ListaCmd>
                    | vazio
<ChamadaFuncao>     -> id (<ListaParametros>)
<ListaParametros>   -> <ListaParametros'>
                    | vazio
<ListaParametros'>  -> <Expressao><ListaParametros''>
<ListaParametros''> -> , <ListaParametros'>
                    | vazio
<Comando>           -> return <TvzExpressao>;
                    | if (<ExpressaoLogica>) <Bloco> <Senao>
                    | while (<ExpressaoLogica>) <Bloco>
                    | id = <Expressao>;
                    | print (<Expressao>);
                    | read (id);
                    | <ChamadaFuncao>;
<TvzExpressao>      -> <Expressao>
                    | vazio
<Senao>             -> else <Bloco>
                    | vazio
-}

import Text.Parsec
import Text.Parsec.Token as T

type Id = String
data Tipo = TDouble | TInt | TString | TVoid deriving Show
data TCons = CDouble Double | CInt Int deriving Show
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

-- <Programa>          -> <ListaFuncoes> <BlocoPrincipal>
parserPrograma :: Parser Programa
parserPrograma = do {
  funcoes <- parserListaFuncoes; 
  blocoPrincipal <- parserBlocoPrincipal;
  let procedimentos = extrairProcs blocoPrincipal;
  let variaveis = extrairVars blocoPrincipal;
  return $ Prog funcoes procedimentos variaveis blocoPrincipal;
}

-- <ListaFuncoes>      -> <Funcao> <ListaFuncoes>
--                     | vazio
parserListaFuncoes :: Parser [Funcao]
parserListaFuncoes = do {f <- parserFuncao; fs <- parserListaFuncoes; return f:fs}
                  <|> return []

-- <Funcao>            -> <TipoRetorno> id (<DeclParametros>) <BlocoPrincipal>
parserFuncao :: Parser Funcao
parserFuncao = do {
  tipoRetorno <- parserTipoRetorno; 
  id <- identifier; 
  char '('; 
  params <- parserDeclParametros; 
  char ')';
  parserBlocoPrincipal;
  return $ id :->: (params, tipoRetorno)  
}

-- <TipoRetorno>       -> <Tipo>
--                     | void
parserTipoRetorno :: Parser Tipo
parserTipoRetorno = do {t <- parserTipo; return t} <|> return "void"

-- <DeclParametros>    -> <Tipo> id <Parametros>
--                     | void
parserDeclParametros :: Parser [Var]
parserDeclParametros = do {
  t <- parserTipo;
  id <- identifier;
  ps <- parserParametros; 
  return $ (id :#: t) : ps
} <|> do {string "void"; return []}

-- <Parametros>        -> , <DeclParametros>
--                     | vazio
parserParametros :: Parser [Var]
parserParametros = do {
  char ',';
  ps <- parserDeclParametros;
  return ps;
} <|> return []

-- <BlocoPrincipal>    -> {<BlocoPrincipal'>}
parserBlocoPrincipal :: Parser Bloco
parserBlocoPrincipal = do {char '{'; b <- parserBlocoPrincipal'; char '}'; return b}

-- <BlocoPrincipal'>   -> <Declaracoes> <ListaCmd>
parserBlocoPrincipal' :: Parser Bloco
parserBlocoPrincipal' = do {parserDeclaracoes; c <- parserListaCmd; return c}

-- <Declaracoes>       -> <Tipo> <ListaId>; <Declaracoes>
--                     | vazio
declaracoes = do {tipo; listaId; declaracoes}
              <|> return id

-- <Tipo>              -> int
--                     | string
--                     | double
parserTipo :: Parser Tipo
parserTipo = do {t <- string "int"; return t}
            <|> do {t <- string "string"; return t}
            <|> do {t <- string "double"; return t}

-- <ListaId>           -> id <ListaId'>
-- <ListaId'>          -> , <ListaId>
--                     | vazio
-- <Bloco>             -> {<ListaCmd>}
-- <ListaCmd>          -> <Comando> <ListaCmd>
--                     | vazio
parserListaCmd :: Parser [Comando]
parserListaCmd = do {c <- parserComando; cs <- parserListaCmd; return $ c : cs}
                <|> return []

-- <ChamadaFuncao>     -> id (<ListaParametros>)
-- <ListaParametros>   -> <ListaParametros'>
--                     | vazio
-- <ListaParametros'>  -> <Expressao><ListaParametros''>
-- <ListaParametros''> -> , <ListaParametros'>
--                     | vazio
-- <Comando>           -> return <TvzExpressao>;
--                     | if (<ExpressaoLogica>) <Bloco> <Senao>
--                     | while (<ExpressaoLogica>) <Bloco>
--                     | id = <Expressao>;
--                     | print (<Expressao>);
--                     | read (id);
parserComando :: Parser Comando
parserComando = do {string "return"; tvzExpr <- parserTvzExpressao; return Imp tvzExpr}
                <|> do {string "if"; char '('; exprL <- parserExpressaoLogica; char ')'; b <- parserBloco; s <- parserSenao; return $ If exprL b s}
                <|> do {string "while"; char '('; exprL <- parserExpressaoLogica; char ')'; b <- parserBloco; return $ While exprL b}
                <|> do {}

--                     | <ChamadaFuncao>;
-- <TvzExpressao>      -> <Expressao>
--                     | vazio
-- <Senao>             -> else <Bloco>
--                     | vazio


