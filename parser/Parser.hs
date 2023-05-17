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
             | literal                          (Lit String) (?)

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

<Expressao'> -> + <Expressao> <Expressao'>
              | - <Expressao> <Expressao'>
              | * <Expressao> <Expressao'>
              | / <Expressao> <Expressao'>
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
module Parser where

import Text.Parsec
import Text.Parsec.Token as T
import ParserProps

-- <Programa>          -> <ListaFuncoes> <BlocoPrincipal>
parserPrograma :: Parser u Programa
-- parserPrograma = do {
--   T.whiteSpaces;
--   funcoes <- parserListaFuncoes; 
--   blocoPrincipal <- parserBlocoPrincipal;
--   let procedimentos = extrairProcs blocoPrincipal
--   variaveis = extrairVars blocoPrincipal;
--   return $ Prog funcoes procedimentos variaveis blocoPrincipal;
-- }
parserPrograma = do
  funcoes <- parserListaFuncoes
  blocoPrincipal <- parserBlocoPrincipal
  let procedimentos = extrairProcs blocoPrincipal
      variaveis = extrairVars blocoPrincipal
  return $ Prog funcoes procedimentos variaveis blocoPrincipal

-- <ListaFuncoes>      -> <Funcao> <ListaFuncoes>
--                     | vazio
parserListaFuncoes :: Parser u [Funcao]
parserListaFuncoes = do {f <- parserFuncao; fs <- parserListaFuncoes; return $ f : fs}
                  <|> return []

-- <Funcao>            -> <TipoRetorno> id (<DeclParametros>) <BlocoPrincipal>
parserFuncao :: Parser u Funcao
parserFuncao = do {
  tipoRetorno <- parserTipoRetorno; 
  id <- identifierToken; 
  char '('; 
  params <- parserDeclParametros; 
  char ')';
  parserBlocoPrincipal;
  return $ id :->: (params, tipoRetorno)  
}

-- <TipoRetorno>       -> <Tipo>
--                     | void
parserTipoRetorno :: Parser u Tipo
parserTipoRetorno = do {parserTipo} <|> return TVoid

-- <DeclParametros>    -> <Tipo> id <Parametros>
--                     | void
parserDeclParametros :: Parser u [Var]
parserDeclParametros = do {
  t <- parserTipo;
  id <- identifierToken;
  ps <- parserParametros; 
  return $ (id :#: t) : ps
} <|> do {string "void"; return []}

-- <Parametros>        -> , <DeclParametros>
--                     | vazio
parserParametros :: Parser u [Var]
parserParametros = do {
  commaToken;
  parserDeclParametros;
} <|> return []

-- <BlocoPrincipal>    -> {<BlocoPrincipal'>}
parserBlocoPrincipal :: Parser u Bloco
parserBlocoPrincipal = do {char '{'; b <- parserBlocoPrincipal'; char '}'; return b}

-- <BlocoPrincipal'>   -> <Declaracoes> <ListaCmd>
parserBlocoPrincipal' :: Parser u Bloco
parserBlocoPrincipal' = do {parserDeclaracoes; parserListaCmd}

-- <Declaracoes>       -> <Tipo> <ListaId>; <Declaracoes>
--                     | vazio
parserDeclaracoes :: Parser u [(Tipo, [Id])]
parserDeclaracoes = do {
  t <- parserTipo; 
  ids <- parserListaId;
  semiToken;
  ds <- parserDeclaracoes; 
  return $ (t, ids) : ds
} <|> return []


-- <Tipo>              -> int
--                     | string
--                     | double
parserTipo :: Parser u Tipo
parserTipo = do {reservedToken "int"; return TInt}
            <|> do {reservedToken "string"; return TString}
            <|> do {reservedToken "double"; return TDouble}

-- <ListaId>           -> id <ListaId'>
parserListaId :: Parser u [Id]
parserListaId = do {id <- identifierToken; ls <- parserListaId'; return $ id : ls}

-- <ListaId'>          -> , <ListaId>
--                     | vazio
parserListaId' :: Parser u [Id]
parserListaId' = do {commaToken; parserListaId}
                 <|> return []

-- <Bloco>             -> {<ListaCmd>}
parserBloco :: Parser u Bloco
parserBloco = do {bracesToken parserListaCmd}

-- <ListaCmd>          -> <Comando> <ListaCmd>
--                     | vazio
parserListaCmd :: Parser u [Comando]
parserListaCmd = do {c <- parserComando; cs <- parserListaCmd; return $ c : cs}
                <|> return []

-- <ChamadaFuncao>     -> id (<ListaParametros>)
parserChamadaFuncao :: Parser u Comando 
parserChamadaFuncao = do {id <- identifierToken; char '('; l <- parserListaParametros; char ')'; return $ Proc id l}

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
parserComando = do {reservedToken "return"; tvzExpr <- parserTvzExpressao; semiToken; return $ Ret tvzExpr}
                <|> do {reservedToken "if"; exprL <- parensToken parserExpressaoLogica; b <- parserBloco; If exprL b <$> parserSenao}
                <|> do {reservedToken "while"; exprL <- parensToken parserExpressaoLogica; While exprL <$> parserBloco}
                <|> do {id <- identifierToken; reservedToken "="; expr <- parserExpressao; semiToken; return $ Atrib id expr}
                <|> do {reservedToken "print"; expr <- parensToken parserExpressao; semiToken; return $ Imp expr}
                <|> do {reservedToken "read"; id <- parensToken identifierToken; semiToken; return $ Leitura id}
                <|> do {c <- parserChamadaFuncao; semiToken; return c}

-- <TvzExpressao>      -> <Expressao>
--                     | vazio
parserTvzExpressao :: Parser u (Maybe Expr)
parserTvzExpressao = do {Just <$> parserExpressao}
                     <|> return Nothing

-- <Senao>             -> else <Bloco>
--                     | vazio
parserSenao :: Parser u Bloco
parserSenao = do {reservedToken "else"; parserBloco}
              <|> return []

-- <Expressao> -> - <Expressao> <Expressao'>        (Neg Expr)
--             | const <Expressao'>                 (Const TCons)
--             | id <Expressao'>                    (IdVar Id)
--             | id <ListaParametros> <Expressao'>  (Chamada Id [Expr])
--             | "string" 
parserExpressao :: Parser u Expr
parserExpressao = do {reservedOpToken "-"; e <- parserExpressao; parserExpressao'; return $ Neg e}
                <|> do {c <- naturalOrFloatToken; parserExpressao'; return $ Const c}
                <|> do {id <- identifierToken; parserExpressao'; return $ IdVar id}
                <|> do {id <- identifierToken; l <- parserListaParametros; parserExpressao'; return $ Chamada id l}
                <|> do {char '"'; s <- stringLiteral; char '"'; return s}

-- <Expressao'> -> + <Expressao> <Expressao'>
--               | - <Expressao> <Expressao'>
--               | * <Expressao> <Expressao'>
--               | / <Expressao> <Expressao'>
--               | vazio
parserExpressao' :: Parser u (Maybe Expr)
parserExpressao' = do {reservedOpToken "+"; parserExpressao; parserExpressao'}
                <|> do {reservedOpToken "-"; parserExpressao; parserExpressao'}
                <|> do {reservedOpToken "*"; parserExpressao; parserExpressao'}
                <|> do {reservedOpToken "/"; parserExpressao; parserExpressao'}
                <|> return Nothing

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
