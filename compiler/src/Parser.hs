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
<ListaParametros'>  -> <Expressao> <ListaParametros''>
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

<Expressao> -> <Expressao> + <Expressao>                        (Expr :+: Expr)
             | <Expressao> - <Expressao>                        (Expr :-: Expr)
             | <Expressao> * <Expressao>                        (Expr :*: Expr)
             | <Expressao> / <Expressao>                        (Expr :/: Expr)
             | - <Expressao>                                    (Neg Expr)
             | const                                            (Const TCons)
             | id                                               (IdVar Id)
             | <ChamadaFuncao>                                  (Chamada Id [Expr])
             | literal                                          (Lit String)

<ExpressaoRelacional> -> <Expressao> == <Expressao>             (Expr :==: Expr)
                       | <Expressao> /= <Expressao>             (Expr :/=: Expr)
                       | <Expressao> < <Expressao>              (Expr :<: Expr)
                       | <Expressao> > <Expressao>              (Expr :>: Expr)
                       | <Expressao> <= <Expressao>             (Expr :<=: Expr)
                       | <Expressao> >= <Expressao>             (Expr :>=: Expr)

<ExpressaoLogica> -> <ExpressaoLogica> & <ExpressaoLogica>      (ExprL :&: ExprL)
                   | <ExpressaoLogica> | <ExpressaoLogica>      (ExprL :|: ExprL)
                   | ! <ExpressaoLogica>                        (Not ExprL)
                   | <ExpressaoRelacional>                      (Rel ExprR)

Sem recursão à esquerda:

<Expressao> -> - <Expressao> <Expressao'>
            | const <Expressao'>
            | id <Expressao'>
            | <ChamadaFuncao> <Expressao'>
            | literal <Expressao'>

<Expressao'> -> + <Expressao> <Expressao'>
              | - <Expressao> <Expressao'>
              | * <Expressao> <Expressao'>
              | / <Expressao> <Expressao'>
              | vazio

<ExpressaoRelacional> -> <Expressao> == <Expressao>
                       | <Expressao> != <Expressao>
                       | <Expressao> < <Expressao>
                       | <Expressao> > <Expressao>
                       | <Expressao> <= <Expressao>
                       | <Expressao> >= <Expressao>


<ExpressaoLogica> -> ! <ExpressaoLogica> <ExpressaoLogica'>
                   | <ExpressaoRelacional> <ExpressaoLogica'>

<ExpressaoLogica'> -> & <ExpressaoLogica> <ExpressaoLogica'>
                    | | <ExpressaoLogica> <ExpressaoLogica'>
                    | vazio
                    
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where

import Text.Parsec ( (<|>), try )
import Text.Parsec.Token as T ()
import CompilerProps
    ( bracesToken,
      commaToken,
      extractProgramTriple,
      getVars,
      identifierToken,
      naturalOrFloatToken,
      parensToken,
      reservedOpToken,
      reservedToken,
      semiToken,
      stringLiteralToken,
      Bloco,
      Comando(..),
      Expr((:/:), Chamada, IdVar, Const, Lit, (:+:), (:-:), (:*:)),
      ExprL(..),
      ExprR(..),
      Funcao(..),
      Id,
      Parser,
      Programa(..),
      TCons(CDouble, CInt),
      Tipo(..),
      Var(..), precedenceTable )
import Data.List (unzip4)
import qualified GHC.Generics as Expressao
import Text.Parsec.Expr (buildExpressionParser)

import Control.Monad.IO.Class (MonadIO(liftIO))

-- <Programa>          -> <ListaFuncoes> <BlocoPrincipal>
parserPrograma :: Parser u Programa
-- Prog [Funcao] [(Id, [Var], Bloco)] [Var] Bloco
parserPrograma = do { fBlocks <- parserListaFuncoes
                    ; (var, bloco) <- parserBlocoPrincipal 
                    ; let (f, _, _) = unzip3 fBlocks
                          t = extractProgramTriple fBlocks
                    ; return $ Prog f t var bloco }

-- <ListaFuncoes>      -> <Funcao> <ListaFuncoes>
--                     | vazio
parserListaFuncoes :: Parser u [(Funcao, [Var], Bloco)]
parserListaFuncoes = do { f <- parserFuncao
                        ; fs <- parserListaFuncoes
                        ; return $ f : fs }
                 <|> do { return [] }

-- <Funcao>            -> <TipoRetorno> id (<DeclParametros>) <BlocoPrincipal>
parserFuncao :: Parser u (Funcao, [Var], Bloco)
parserFuncao = do { tipoRetorno <- parserTipoRetorno
                  ; id <- identifierToken
                  ; params <- parensToken parserDeclParametros
                  ; (vars, bloco) <- parserBlocoPrincipal
                  ; let f = id :->: (params, tipoRetorno)
                  ; return (f, vars, bloco) }

-- <TipoRetorno>       -> <Tipo>
--                     | void
parserTipoRetorno :: Parser u Tipo
parserTipoRetorno = do { parserTipo } 
                <|> do { reservedToken "void"
                       ; return TVoid }

-- <DeclParametros>    -> <Tipo> id <Parametros>
--                     | void
parserDeclParametros :: Parser u [Var]
parserDeclParametros = do {
  t <- parserTipo;
  id <- identifierToken;
  ps <- parserParametros; 
  return $ (id :#: t) : ps
} <|> do { reservedToken "void"
         ; return [] }

-- <Parametros>        -> , <DeclParametros>
--                     | vazio
parserParametros :: Parser u [Var]
parserParametros = do {
  commaToken;
  parserDeclParametros;
} <|> return []

-- <BlocoPrincipal>    -> {<BlocoPrincipal'>}
parserBlocoPrincipal :: Parser u ([Var], Bloco)
parserBlocoPrincipal = do { bracesToken parserBlocoPrincipal' }

-- <BlocoPrincipal'>   -> <Declaracoes> <ListaCmd>
parserBlocoPrincipal' :: Parser u ([Var], Bloco)
parserBlocoPrincipal' = do { declaracoes <- parserDeclaracoes
                           ; cmds <- parserListaCmd
                           ; return (declaracoes, cmds) }

-- <Declaracoes>       -> <Tipo> <ListaId>; <Declaracoes>
--                     | vazio
parserDeclaracoes :: Parser u [Var]
parserDeclaracoes = do { t <- parserTipo
                       ; ids <- parserListaId
                       ; semiToken
                       ; ds <- parserDeclaracoes
                       ; return $ getVars (t, ids) ++ ds
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
parserChamadaFuncao = do { id <- identifierToken
                         ; l <- parensToken parserListaParametros
                         ; return $ Proc id l }

-- <ListaParametros>   -> <ListaParametros'>
--                     | vazio
parserListaParametros :: Parser u [Expr]
parserListaParametros = do { parserListaParametros' }
                        <|> do { return [] }

-- <ListaParametros'>  -> <Expressao> <ListaParametros''>
parserListaParametros' :: Parser u [Expr]
parserListaParametros' = do { expr <- parserExpressao
                            ; l'' <- parserListaParametros''
                            ; return $ expr : l'' }

-- <ListaParametros''> -> , <ListaParametros'>
--                     | vazio
parserListaParametros'' :: Parser u [Expr]
parserListaParametros'' = do { commaToken
                             ; parserListaParametros' }
                          <|> do { return [] }

-- <Comando>           -> return <TvzExpressao>;
--                     | if (<ExpressaoLogica>) <Bloco> <Senao>
--                     | while (<ExpressaoLogica>) <Bloco>
--                     | id = <Expressao>;
--                     | print (<Expressao>);
--                     | read (id);
--                     | <ChamadaFuncao>;
parserComando :: Parser u Comando
parserComando = do { reservedToken "return"
                   ; tvzExpr <- parserTvzExpressao
                   ; semiToken
                   ; return $ Ret tvzExpr }
            <|> do { reservedToken "if"
                   ; exprL <- parensToken parserExpressaoLogica
                   ; b <- parserBloco
                   ; If exprL b <$> parserSenao }
            <|> do { reservedToken "while"
                   ; exprL <- parensToken parserExpressaoLogica
                   ; While exprL <$> parserBloco }
            <|> try (do { id <- identifierToken
                   ; reservedToken "="
                   ; expr <- parserExpressao
                   ; semiToken
                   ; return $ Atrib id expr })
            <|> do { reservedToken "print"
                   ; expr <- parensToken parserExpressao
                   ; semiToken
                   ; return $ Imp expr }
            <|> do { reservedToken "read"
                   ; id <- parensToken identifierToken
                   ; semiToken
                   ; return $ Leitura id }
            <|> do { c <- parserChamadaFuncao
                   ; semiToken
                   ; return c }

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

parserExpressao :: Parser u Expr
parserExpressao = buildExpressionParser precedenceTable (
                  try (do { (Proc id l) <- parserChamadaFuncao
                          ; return $ Chamada id l })
              <|> do { id <- identifierToken
                     ; return $ IdVar id }
              <|> do { reservedToken "-"
                     ; parserExpressao }
              <|> do { const <- naturalOrFloatToken
                     ; let c = case const of
                            Left n -> Const $ CInt n
                            Right d -> Const $ CDouble d
                     ; return c }
              <|> do { s <- stringLiteralToken
                     ; return $ Lit s })

-- <ExpressaoRelacional> -> <Expressao> <ExpressaoRelacional'>
parserExpressaoRelacional :: Parser u ExprR
parserExpressaoRelacional = do { e1 <- parserExpressao 
                               ; parserExpressaoRelacional' e1 }

-- <ExpressaoRelacional'> -> == <Expressao>
--                         | != <Expressao>
--                         | < <Expressao>
--                         | > <Expressao>
--                         | <= <Expressao>
--                         | >= <Expressao>
parserExpressaoRelacional' :: Expr -> Parser u ExprR
parserExpressaoRelacional' e1 = do { reservedToken "=="
                                ; e2 <- parserExpressao 
                                ; return $ e1 :==: e2 }
                            <|> do { reservedToken "!="
                                ; e2 <- parserExpressao
                                ; return $ e1 :/=: e2 }
                            <|> do { reservedToken "<="
                                ; e2 <- parserExpressao
                                ; return $ e1 :<=: e2 }
                            <|> do { reservedToken ">="
                                ; e2 <- parserExpressao
                                ; return $ e1 :>=: e2 }
                            <|> do { reservedToken "<"
                                ; e2 <- parserExpressao
                                ; return $ e1 :<: e2 }
                            <|> do { reservedToken ">"
                                ; e2 <- parserExpressao
                                ; return $ e1 :>: e2 }



-- <ExpressaoLogica> -> ! <ExpressaoLogica> <ExpressaoLogica'>
--                    | <ExpressaoRelacional> <ExpressaoLogica'>
parserExpressaoLogica :: Parser u ExprL
parserExpressaoLogica = do { reservedToken "!"
                           ; exprL <- parserExpressaoLogica
                           ; parserExpressaoLogica' exprL
                           ; return $ Not exprL }
                    <|> do { exprR <- parserExpressaoRelacional
                           ; parserExpressaoLogica' (Rel exprR)
                           ; return $ Rel exprR }


-- A SER EXCLUÍDO
-- -- <ExpressaoLogica'> -> & <ExpressaoLogica'>
-- --                     | | <ExpressaoLogica'>
-- --                     | vazio
-- parserExpressaoLogica' :: Parser u (Maybe ExprL)
-- parserExpressaoLogica' = do {reservedToken "&"; parserExpressaoLogica'}
--                      <|> do {reservedToken "|"; parserExpressaoLogica'}
--                      <|> do return Nothing

-- <ExpressaoLogica'> -> & <ExpressaoLogica> <ExpressaoLogica'>
--                     | | <ExpressaoLogica> <ExpressaoLogica'>
--                     | vazio
parserExpressaoLogica' :: ExprL -> Parser u (Maybe ExprL)
parserExpressaoLogica' exprL1 = do { reservedToken "&"
                                   ; exprL2 <- parserExpressaoLogica
                                   ; parserExpressaoLogica' exprL2
                                   ; return $ Just (exprL1 :&: exprL2) }
                            <|> do { reservedToken "|"
                                   ; exprL2 <- parserExpressaoLogica
                                   ; parserExpressaoLogica' exprL2
                                   ; return $ Just (exprL1 :|: exprL2) }
                            <|> do return Nothing

