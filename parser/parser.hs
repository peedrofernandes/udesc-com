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



Regras de derivação personalizadas

<Expressao> -> <Expressao> + <Expressao>        (Expr :+: Expr)
             | <Expressao> - <Expressao>        (Expr :-: Expr)
             | <Expressao> * <Expressao>        (Expr :*: Expr)
             | <Expressao> / <Expressao>        (Expr :/: Expr)
             | - <Expressao>                    (Neg Expr)
             | const                            (Const TCons)
             | id                               (IdVar Id)
             | id <ListaParametros>             (Chamada Id [Expr])
             |                                  (Lit String) (?)

<ExpressaoRelacional> -> <Expressao> == <Expressao>
                       | <Expressao> /= <Expressao>
                       | <Expressao> < <Expressao>
                       | <Expressao> > <Expressao>
                       | <Expressao> <= <Expressao>
                       | <Expressao> >= <Expressao>

<ExpressaoLogica> -> <ExpressaoLogica> & <ExpressaoLogica>
                   | <ExpressaoLogica> | <ExpressaoLogica>
                   | ! <ExpressaoLogica>
                   | <ExpressaoRelacional>

Sem recursão à esquerda:

<Expressao> -> - <Expressao> <Expressao'>        (Neg Expr)
            | const <Expressao'>                 (Const TCons)
            | id <Expressao'>                    (idVar Id)
            | id <ListaParametros> <Expressao'>  (Chamada Id [Expr])
            | "string"                           (Lit String) (?)

<Expressao'> -> + <Expressao'>
              | - <Expressao'>
              | * <Expressao'>
              | / <Expressao'>
              | vazio

<ExpressaoRelacional> -> <Expressao> == <Expressao>
                       | <Expressao> /= <Expressao>
                       | <Expressao> < <Expressao>
                       | <Expressao> > <Expressao>
                       | <Expressao> <= <Expressao>
                       | <Expressao> >= <Expressao>


<ExpressaoLogica> -> ! <ExpressaoLogica> <ExpressaoLogica'>
                   | <ExpressaoRelacional> <ExpressaoLogica'>

<ExpressaoLogica'> -> & <ExpressaoLogica'>
                    | | <ExpressaoLogica'>
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

type Parser u r = Parsec String u r

-- <Programa>          -> <ListaFuncoes> <BlocoPrincipal>
parserPrograma :: Parser u Programa
parserPrograma = do {
  T.whiteSpaces
  funcoes <- parserListaFuncoes; 
  blocoPrincipal <- parserBlocoPrincipal;
  let procedimentos = extrairProcs blocoPrincipal;
  let variaveis = extrairVars blocoPrincipal;
  return $ Prog funcoes procedimentos variaveis blocoPrincipal;
}

-- <ListaFuncoes>      -> <Funcao> <ListaFuncoes>
--                     | vazio
parserListaFuncoes :: Parser u [Funcao]
parserListaFuncoes = do {f <- parserFuncao; fs <- parserListaFuncoes; return f:fs}
                  <|> return []

-- <Funcao>            -> <TipoRetorno> id (<DeclParametros>) <BlocoPrincipal>
parserFuncao :: Parser u Funcao
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
parserTipoRetorno :: Parser u Tipo
parserTipoRetorno = do {t <- parserTipo; return t} <|> return "void"

-- <DeclParametros>    -> <Tipo> id <Parametros>
--                     | void
parserDeclParametros :: Parser u [Var]
parserDeclParametros = do {
  t <- parserTipo;
  id <- identifier;
  ps <- parserParametros; 
  return $ (id :#: t) : ps
} <|> do {string "void"; return []}

-- <Parametros>        -> , <DeclParametros>
--                     | vazio
parserParametros :: Parser u [Var]
parserParametros = do {
  char ',';
  ps <- parserDeclParametros;
  return ps;
} <|> return []

-- <BlocoPrincipal>    -> {<BlocoPrincipal'>}
parserBlocoPrincipal :: Parser u Bloco
parserBlocoPrincipal = do {char '{'; b <- parserBlocoPrincipal'; char '}'; return b}

-- <BlocoPrincipal'>   -> <Declaracoes> <ListaCmd>
parserBlocoPrincipal' :: Parser u Bloco
parserBlocoPrincipal' = do {parserDeclaracoes; c <- parserListaCmd; return c}

-- <Declaracoes>       -> <Tipo> <ListaId>; <Declaracoes>
--                     | vazio
declaracoes = do {tipo; listaId; declaracoes}
              <|> return id

-- <Tipo>              -> int
--                     | string
--                     | double
parserTipo :: Parser u Tipo
parserTipo = do {t <- string "int"; return t}
            <|> do {t <- string "string"; return t}
            <|> do {t <- string "double"; return t}

-- <ListaId>           -> id <ListaId'>
-- <ListaId'>          -> , <ListaId>
--                     | vazio
-- <Bloco>             -> {<ListaCmd>}
-- <ListaCmd>          -> <Comando> <ListaCmd>
--                     | vazio
parserListaCmd :: Parser u [Comando]
parserListaCmd = do {c <- parserComando; cs <- parserListaCmd; return $ c : cs}
                <|> return []

-- <ChamadaFuncao>     -> id (<ListaParametros>)
parserChamadaFuncao :: Parser u Comando 
parserChamadaFuncao = do {id <- identifier; char '('; l <- parserListaParametros; char ')'; return $ Proc id l}

-- <ListaParametros>   -> <ListaParametros'>
--                     | vazio
parserListaParametros :: Parser u [Expr]
parserListaParametros = do {l' <- parserListaParametros'; return l'}
                        <|> do {return []}

-- <ListaParametros'>  -> <Expressao> <ListaParametros''>
parserListaParametros' :: Parser u [Expr]
parserListaParametros' = do {expr <- parserExpressao; l'' <- parserListaParametros''; return $ expr : l''}

-- <ListaParametros''> -> , <ListaParametros'>
--                     | vazio
parserListaParametros'' :: Parser u [Expr]
parserListaParametros'' = do {char ','; l' <- parserListaParametros'; return l'}
                          <|> do {return []}

-- <Comando>           -> return <TvzExpressao>;
--                     | if (<ExpressaoLogica>) <Bloco> <Senao>
--                     | while (<ExpressaoLogica>) <Bloco>
--                     | id = <Expressao>;
--                     | print (<Expressao>);
--                     | read (id);
--                     | <ChamadaFuncao>;
parserComando :: Parser u Comando
parserComando = do {string "return"; tvzExpr <- parserTvzExpressao; return $  Ret tvzExpr}
                <|> do {string "if"; char '('; exprL <- parserExpressaoLogica; char ')'; b <- parserBloco; s <- parserSenao; return $ If exprL b s}
                <|> do {string "while"; char '('; exprL <- parserExpressaoLogica; char ')'; b <- parserBloco; return $ While exprL b}
                <|> do {id <- identifier; char '='; expr <- parserExpressao; char ';'; return $ Atrib id expr}
                <|> do {string "print"; char '('; expr <- parserExpressao; char ')'; char ';'; return $ Imp expr}
                <|> do {string "read"; char '('; id <- identifier; char ')'; char ';'; return $ Leitura id}
                <|> do {c <- parserChamadaFuncao; return c}
-- <TvzExpressao>      -> <Expressao>
--                     | vazio
-- <Senao>             -> else <Bloco>
--                     | vazio

-- <Expressao> -> - <Expressao> <Expressao'>        (Neg Expr)
--             | const <Expressao'>                 (Const TCons)
--             | id <Expressao'>                    (IdVar Id)
--             | id <ListaParametros> <Expressao'>  (Chamada Id [Expr])
--             | "string" 
parserExpressao :: Parser u Expr
parserExpressao = do {char '-'; e <- parserExpressao; parserExpressao'; return $ Neg e}
                <|> do {c <- many digit; parserExpressao'; return $ Const c}
                <|> do {id <- identifier; parserExpressao'; return $ IdVar id}
                <|> do {id <- identifier; l <- parserListaParametros; parserExpressao'; return $ Chamada id l}
                <|> do {char '"'; s <- ; char '"'; return s}

-- <Expressao'> -> + <Expressao'>
--               | - <Expressao'>
--               | * <Expressao'>
--               | / <Expressao'>
--               | vazio

-- <ExpressaoRelacional> -> <Expressao> == <Expressao>
--                        | <Expressao> /= <Expressao>
--                        | <Expressao> < <Expressao>
--                        | <Expressao> > <Expressao>
--                        | <Expressao> <= <Expressao>
--                        | <Expressao> >= <Expressao>


-- <ExpressaoLogica> -> ! <ExpressaoLogica> <ExpressaoLogica'>
--                    | <ExpressaoRelacional> <ExpressaoLogica'>

-- <ExpressaoLogica'> -> & <ExpressaoLogica'>
--                     | | <ExpressaoLogica'>
--                     | vazio

-- Dúvida
-- Usar a tabela de tokens
-- Limpar espaços antes de iniciar a análise sintática
-- Análise da montagem das regras de produção personalizadas
-- O que seria um Lit
