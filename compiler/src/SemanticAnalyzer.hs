module SemanticAnalyzer where

import Control.Monad.State
import Control.Monad.Writer
import CompilerProps
import Debug.Trace

import qualified Data.Map as Map

type SymbolTable = Map.Map Id (Either Tipo ([Var], Tipo))
type SymbolTableState a = StateT (SymbolTable, [String]) IO a

warn :: String -> SymbolTableState ()
warn warning = modify (\(st, warnings) -> (st, warnings ++ ["Warning: " ++ warning]))

-- O analisador semântico deve receber como entrada a AST, representada pelo tipo de
-- dado algébrico Programa, fazer a verificação de tipos e retornar uma AST
-- correspondente incluindo as coerções de tipos, erros e advertências deverão ser emitidos
-- no processo. As regras para coerção de tipos e emissão de mensagens de erro são:


-- - Expressões com tipos incompatíveis devem emitir mensagens de erro.
-- ------------------------------------
-- - Em expressões binárias aritméticas ou relacionais quando um dos operandos for
-- do tipo int e o outro for do tipo double o operando do tipo int deve ser
-- convertido à double.
-- ------------------------------------

getExprType :: Expr -> SymbolTableState Tipo
getExprType expr = do
  (table, warnings) <- get -- Obter o estado na tabela de símbolos, que armazena nomes de funções e variáveis
  case expr of
    e1 :+: e2 -> getBinaryArithmeticExprType e1 e2
    e1 :-: e2 -> getBinaryArithmeticExprType e1 e2
    e1 :*: e2 -> getBinaryArithmeticExprType e1 e2
    e1 :/: e2 -> getBinaryArithmeticExprType e1 e2
    Neg e -> getExprType e
    Const c -> case c of
      CDouble c -> return TDouble
      CInt c -> return TInt
    IdVar id -> do
      let elemFound = Map.lookup id table
      case elemFound of -- Pegar o id na tabela de símbolos
        Just (Left varType) -> return varType -- O valor encontrado na tabela é uma variável
        Just (Right functionTuple) -> error "Variável usada não declarada" -- O valor encontrado na tabela é uma função
        Nothing -> error "Variável usada não declarada" -- O id usado na expressão não existe na tabela
    Chamada id es -> do
      case Map.lookup id table of -- Pegar o id na tabela de símbolos
        Just (Left varType) -> error "Função usada não declarada" -- O valor encontrado na tabela é uma variável
        Just (Right (_, functionReturnType)) -> return functionReturnType -- O valor encontrado na tabela é uma função
        Nothing -> error "Função usada não declarada" -- O id usado na expressão não existe na tabela
    Lit _ -> return TString
    IntDouble _ -> return TDouble
    DoubleInt _ -> return TInt

getBinaryArithmeticExprType :: Expr -> Expr -> SymbolTableState Tipo
getBinaryArithmeticExprType e1 e2 = do
  t1 <- getExprType e1
  t2 <- getExprType e2
  case t1 of
    TDouble -> case t2 of
      TDouble -> return TDouble 
      TInt -> return TDouble
      _ -> error "Erro: Tipos incompatíveis na expressão aritmética"
    TInt -> case t2 of
      TInt -> return TInt
      TDouble -> return TDouble
      _ -> error "Erro: Tipos incompatíveis na expressão aritmética"
    _ -> error "Erro: Tipos inválidos na expressão aritmética"
      
checkExpr :: Expr -> SymbolTableState Expr
checkExpr expr  = do
  (table, warnings) <- get
  case expr of
    e1 :+: e2 -> checkBinaryExpr e1 e2 (:+:)
    e1 :-: e2 -> checkBinaryExpr e1 e2 (:-:)
    e1 :*: e2 -> checkBinaryExpr e1 e2 (:*:)
    e1 :/: e2 -> checkBinaryExpr e1 e2 (:/:)
    Neg e -> checkExpr e
    Const c -> return expr
    IdVar id -> do
      let elemFound = Map.lookup id table
      case elemFound of
        Just (Left varType) -> return expr
        Just (Right functionTuple) -> error "Variável usada não declarada"
        Nothing -> error "Variável usada não declarada"
    Chamada id es -> do
      case Map.lookup id table of
        Just (Left varType) -> error "Função usada não declarada"
        Just (Right (_, functionReturnType)) -> do 
          checkedExprs <- sequence [checkExpr e | e <- es]
          return $ Chamada id checkedExprs
        Nothing -> error "Função usada não declarada"
    Lit _ -> return expr
    IntDouble _ -> return expr
    DoubleInt _ -> return expr
  
checkBinaryExpr :: Expr -> Expr -> (Expr -> Expr -> Expr) -> SymbolTableState Expr
checkBinaryExpr e1 e2 constructor = do
  fixedExpr1 <- checkExpr e1
  fixedExpr2 <- checkExpr e2
  t1 <- getExprType fixedExpr1
  t2 <- getExprType fixedExpr2
  case t1 of 
    TDouble -> case t2 of
      TDouble -> return $ constructor fixedExpr1 fixedExpr2
      TInt -> return $ constructor fixedExpr1 (IntDouble fixedExpr2)
      _ -> error "Erro: Tipos incompatíveis na expressão aritmética"
    TInt -> case t2 of
      TInt -> return $ constructor fixedExpr1 fixedExpr2
      TDouble -> return $ constructor (IntDouble fixedExpr1) fixedExpr2
      _ -> error "Erro: Tipos imcompatíveis na expressão aritmética"
    _ -> error "Erro: Tipos imcompatíveis na expressão aritmética"



-- - Quando uma variável declarada como double receber o valor de uma expressão
-- de tipo int, o resultado da expressão deve ser convertido para o tipo double. Isso
-- é válido para comandos de atribuição, passagem de parâmetros em chamadas de
-- funções e para o retorno de funções.
-- ------------------------------------
-- - Quando uma variável declarada como int receber o valor de uma expressão de
-- tipo double, o resultado da expressão deve ser convertido para o tipo int, nesse
-- caso deve ser emitida uma mensagem de advertência. Isso é válido para
-- comandos de atribuição, passagem de parâmetros em chamadas de funções e
-- para o retorno de funções.
-- ------------------------------------
-- - Atribuição de variáveis ou retorno de funções com tipos conflitantes devem
-- ocasionar a emissão de mensagens de erro.
-- ------------------------------------
checkComando :: Comando -> SymbolTableState Comando
checkComando cmd = do
  (table, warnings) <- get
  case cmd of
    Atrib id expr -> do
      let elemFound = Map.lookup id table
      case elemFound of
        Just (Left varType) -> do 
          exprType <- getExprType expr
          case (varType, exprType) of
            (TDouble, TInt) -> return $ Atrib id (IntDouble expr)
            (TInt, TDouble) -> do
              warn $ "Variável " ++ show id ++ ", do tipo Int, recebeu um valor double"
              return $ Atrib id (DoubleInt expr)
            (TInt, TInt) -> return cmd
            (TDouble, TDouble) -> return cmd
            (TVoid, TVoid) -> return cmd
            (TString, TString) -> return cmd
            _ -> error $ "Erro: Atribuição de tipo " ++ show exprType ++ " à variável de tipo " ++ show varType
        Just (Right fn) -> error $ "Erro: Atribuição ao id " ++ show id ++ ", que é uma função"
        Nothing -> error $ "Erro: Atribuição à variável " ++ show id ++ " não declarada"
    Proc id exprs -> do
      let elemFound = Map.lookup id table
      case elemFound of
        Just (Left varType) -> error $ "Função " ++ show id ++ " não encontrada"
        Just (Right (vars, returnType)) -> do
          exprTypes <- mapM getExprType exprs
          let varTypes = map (\(_ :#: t) -> t) vars
          if exprTypes == varTypes then return cmd
            else error $ "Erro: Parâmetros inválidos na chamada da função " ++ id 
    Ret mybExpr -> do
      let elemFound = Map.lookup "this" table
      case elemFound of
        Just (Left varType) -> error "'this' não pode ser usado como nome de variável."
        Just (Right (id, retType)) -> do
          case mybExpr of
            Just expr -> do
              t <- getExprType expr
              if retType == t then return cmd else error $ "Tipo de retorno inválido - Retorno esperado para a função " ++ show id ++ ": " ++ show retType
            Nothing -> if retType == TVoid then return cmd else error $ "Tipo de retorno inválido - Retorno esperado para a função " ++ show id ++ ": " ++ show retType
        Nothing -> error "Erro de escopo"
      
          
      




-- - O tipo string pode ocorrer apenas em expressões relacionais, os dois operandos
-- devem ser do mesmo tipo, caso contrário uma mensagem de erro deve ser
-- emitida.
-- ------------------------------------



-- - Chamadas de funções com número de parâmetros errados ou com parâmetros
-- formais e reais com tipos conflitantes devem ocasionar a emissão de mensagens
-- de erro.
-- ------------------------------------



-- - O uso de variáveis não declaradas deve informado com uma mensagem de erro.
-- ------------------------------------

-- - Chamada de funções não declaradas deve ocasionar a emissão de uma
-- mensagem de erro.
-- ------------------------------------

-- - A existência de variáveis multiplamente declaradas em uma mesma função deve
-- ocasionar a emissão de uma mensagem de erro.
-- ------------------------------------

-- - A existência de funções multiplamente declaradas deve ocasionar uma
-- mensagem de erro.
-- ------------------------------------

-- Tipos de dados que devem ser verificados:
-- Expr (apenas binárias);
-- ExprR (apenas binárias (todas));
-- Var (int para double);
-- 

-- 

-- checkFuncoes :: [Funcao] -> SymbolTableState [Funcao]
-- checkFuncoes [] = return []
-- checkFuncoes (fun@(id :->: ([v], t)) : fs) = 
--   do { tableState <- get
--      ; case Map.lookup id tableState of 
--         Just _ -> error $ "Function " ++ id ++ " is declared twice"
--         Nothing -> do
--           put (Map.insert id t tableState)
--           rest <- checkFuncoes fs
--           return (fun : rest) }

-- semanticAnalyzer :: Programa -> SymbolTableState Programa
-- semanticAnalyzer (Prog f fImpl var bloco) = do
--   let assinaturaFuncoes = checkFuncoes f
--   let 




