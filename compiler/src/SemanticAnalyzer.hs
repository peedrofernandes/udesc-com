module SemanticAnalyzer where

import Control.Monad.State
import Control.Monad.Writer
import CompilerProps
import Debug.Trace

import qualified Data.Map as Map

type SymbolTable = Map.Map Id (Either Tipo ([Var], Tipo))
type SemanticAnalyzerState a = StateT (SymbolTable, [String]) IO a

warn :: String -> SemanticAnalyzerState ()
warn warning = modify (\(st, warnings) -> (st, warnings ++ ["Warning: " ++ warning]))

-- O analisador semântico deve receber como entrada a AST, representada pelo tipo de
-- dado algébrico Programa, fazer a verificação de tipos e retornar uma AST
-- correspondente incluindo as coerções de tipos, erros e advertências deverão ser emitidos
-- no processo. As regras para coerção de tipos e emissão de mensagens de erro são:


-- - Em expressões binárias aritméticas ou relacionais quando um dos operandos for
-- do tipo int e o outro for do tipo double o operando do tipo int deve ser
-- convertido à double.
-- ------------------------------------
-- - O tipo string pode ocorrer apenas em expressões relacionais, os dois operandos
-- devem ser do mesmo tipo, caso contrário uma mensagem de erro deve ser
-- emitida.
-- ------------------------------------
-- - Expressões com tipos incompatíveis devem emitir mensagens de erro.
-- ------------------------------------

getExprType :: Expr -> SemanticAnalyzerState Tipo
getExprType (e1 :+: e2) = getBinaryArithmeticExprType e1 e2 
getExprType (e1 :-: e2) = getBinaryArithmeticExprType e1 e2 
getExprType (e1 :*: e2) = getBinaryArithmeticExprType e1 e2 
getExprType (e1 :/: e2) = getBinaryArithmeticExprType e1 e2 
getExprType (Neg e) = getExprType e
getExprType (Lit _) = return TString
getExprType (IntDouble _) = return TDouble
getExprType (DoubleInt _) = return TInt

getExprType (Const c) = case c of
  CDouble c -> return TDouble
  CInt c -> return TInt

getExprType (IdVar id) = do
  (table, warnings) <- get -- Obter o estado na tabela de símbolos, que armazena nomes de funções e variáveis
  let elemFound = Map.lookup id table -- Pegar o id na tabela de símbolos
  case elemFound of
    Just (Left varType) -> return varType -- O valor encontrado na tabela é uma variável
    Just (Right functionTuple) -> error $ "Erro: " ++ id ++ " é uma função e está sendo usada como variável" -- O valor encontrado na tabela é uma função
    Nothing -> error "Erro: Variável usada não encontrada" -- O id usado na expressão não existe na tabela

getExprType (Chamada id es) = do
  (table, warnings) <- get
  let elemFound = Map.lookup id table -- Pegar o id na tabela de símbolos
  case elemFound of
    Just (Left varType) -> error $ "Erro: " ++ id ++ " é uma variável e está sendo usada como uma função" -- O valor encontrado na tabela é uma variável
    Just (Right (_, functionReturnType)) -> return functionReturnType -- O valor encontrado na tabela é uma função
    Nothing -> error "Erro: Função usada não declarada" -- O id usado na expressão não existe na tabela

getBinaryArithmeticExprType :: Expr -> Expr -> SemanticAnalyzerState Tipo
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
      
checkExpr :: Expr -> SemanticAnalyzerState Expr
checkExpr (e1 :+: e2) = checkBinaryExpr e1 e2 (:+:)
checkExpr (e1 :-: e2) = checkBinaryExpr e1 e2 (:-:)
checkExpr (e1 :*: e2) = checkBinaryExpr e1 e2 (:*:)
checkExpr (e1 :/: e2) = checkBinaryExpr e1 e2 (:/:)
checkExpr (Neg e) = checkExpr e
checkExpr (Const c) = return (Const c)
checkExpr (Lit str) = return (Lit str)
checkExpr (IntDouble e) = IntDouble <$> checkExpr e
checkExpr (DoubleInt e) = DoubleInt <$> checkExpr e

-- - O uso de variáveis não declaradas deve vir informado com uma mensagem de erro.
-- ------------------------------------
checkExpr (IdVar id) = do
  (table, warnings) <- get
  let elemFound = Map.lookup id table
  case elemFound of
    Just (Left varType) -> return (IdVar id)
    Just (Right functionTuple) -> error "Variável usada não declarada"
    Nothing -> error "Variável usada não declarada"

-- - Chamada de funções não declaradas deve ocasionar a emissão de uma
-- mensagem de erro.
-- ------------------------------------
checkExpr (Chamada id exprs) = do
  (table, warnings) <- get
  case Map.lookup id table of
    Just (Left varType) -> error "Função usada não declarada"
    Just (Right (vars, functionReturnType)) -> do
      checkedExprs <- checkFunctionParameters vars exprs id
      return $ Chamada id checkedExprs
    Nothing -> error "Função usada não declarada"
  
checkBinaryExpr :: Expr -> Expr -> (Expr -> Expr -> Expr) -> SemanticAnalyzerState Expr
checkBinaryExpr e1 e2 constructor = do
  fixedExpr1 <- checkExpr e1
  fixedExpr2 <- checkExpr e2
  t1 <- getExprType fixedExpr1
  t2 <- getExprType fixedExpr2
  case (t1, t2) of 
    (TInt, TInt) -> return $ constructor fixedExpr1 fixedExpr2
    (TDouble, TDouble) -> return $ constructor fixedExpr1 fixedExpr2
    (TInt, TDouble) -> return $ constructor (IntDouble fixedExpr1) fixedExpr2
    (TDouble, TInt) -> return $ constructor fixedExpr1 (IntDouble fixedExpr2)
    (_, _) -> error "Erro: Tipos incompatíveis na expressão aritmética"

checkExprR :: ExprR -> SemanticAnalyzerState ExprR
checkExprR (e1 :==: e2) = checkBinaryExprR e1 e2 (:==:)
checkExprR (e1 :/=: e2) = checkBinaryExprR e1 e2 (:/=:)
checkExprR (e1 :<: e2)  = checkBinaryExprR e1 e2 (:<:)
checkExprR (e1 :>: e2)  = checkBinaryExprR e1 e2 (:>:)
checkExprR (e1 :<=: e2) = checkBinaryExprR e1 e2 (:<=:)
checkExprR (e1 :>=: e2) = checkBinaryExprR e1 e2 (:>=:)

checkBinaryExprR :: Expr -> Expr -> (Expr -> Expr -> ExprR) -> SemanticAnalyzerState ExprR
checkBinaryExprR e1 e2 constructor = do
  fixedExpr1 <- checkExpr e1
  fixedExpr2 <- checkExpr e2
  t1 <- getExprType fixedExpr1
  t2 <- getExprType fixedExpr2
  case (t1, t2) of
    (TInt, TInt) -> return $ constructor fixedExpr1 fixedExpr2
    (TInt, TDouble) -> return $ constructor (IntDouble fixedExpr1) fixedExpr2
    (TInt, _) -> error "Erro: Tipos errados na expressão lógica"
    (TDouble, TInt) -> return $ constructor fixedExpr1 (IntDouble fixedExpr2)
    (TDouble, TDouble) -> return $ constructor fixedExpr1 fixedExpr2
    (TDouble, _) -> error "Erro: Tipos errados na expressão lógica"
    (TString, TString) -> return $ constructor fixedExpr1 fixedExpr2
    (_, _) -> error "Erro: Tipos errados na expressão lógica"
    

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
checkComando :: Comando -> SemanticAnalyzerState Comando

checkComando (Atrib id expr) = do
  (table, warnings) <- get
  let elemFound = Map.lookup id table
  case elemFound of
    Just (Left varType) -> do 
      exprType <- getExprType expr
      case (varType, exprType) of
        (TDouble, TInt) -> return $ Atrib id (IntDouble expr)
        (TInt, TDouble) -> do
          warn $ "Variável " ++ show id ++ ", do tipo Int, recebeu um valor double"
          return $ Atrib id (DoubleInt expr)
        (TInt, TInt) -> return (Atrib id expr)
        (TDouble, TDouble) -> return (Atrib id expr)
        (TVoid, TVoid) -> return (Atrib id expr)
        _ -> error $ "Erro: Atribuição de tipo " ++ show exprType ++ " à variável de tipo " ++ show varType
    Just (Right fn) -> error $ "Erro: Atribuição ao id " ++ show id ++ ", que é uma função"
    Nothing -> error $ "Erro: Atribuição à variável " ++ show id ++ " não declarada"

-- - Chamada de funções não declaradas deve ocasionar a emissão de uma
-- mensagem de erro.
-- ------------------------------------
checkComando (Proc id exprs) = do
  (table, warnings) <- get
  let elemFound = Map.lookup id table
  case elemFound of
    Just (Left varType) -> error $ "Função " ++ show id ++ " não declarada"
    Just (Right (vars, returnType)) -> do
      checkedExprs <- checkFunctionParameters vars exprs id
      return $ Proc id checkedExprs
    Nothing -> error $ "Função " ++ show id ++ " não declarada"
  
checkComando (Ret mybExpr) = do
  (table, warnings) <- get
  let elemFound = Map.lookup "this" table
  case elemFound of
    Just (Left varType) -> error "Erro: Há uma variável com o nome reservado 'this'."
    Just (Right (vars, retType)) -> do
      case mybExpr of
        Just expr -> do
          exprType <- getExprType expr
          case (retType, exprType) of
            (TInt, TInt) -> return $ Ret mybExpr
            (TDouble, TDouble) -> return $ Ret mybExpr
            (TString, TString) -> return $ Ret mybExpr
            (TVoid, TVoid) -> return $ Ret mybExpr
            (TInt, TDouble) -> do 
              warn "Função retornou Double quando deveria retornar Int"
              return $ Ret (Just (DoubleInt expr))
            (TDouble, TInt) -> return $ Ret (Just (IntDouble expr))
            (_, _) -> error "Erro: Tipo de retorno inválido"
        Nothing -> if retType == TVoid 
          then return (Ret mybExpr) 
          else error "Erro: Tipo de retorno inválido"
    Nothing -> error "Erro: Erro de escopo"

checkComando cmd = return cmd

-- - Chamadas de funções com número de parâmetros errados ou com parâmetros
-- formais e reais com tipos conflitantes devem ocasionar a emissão de mensagens
-- de erro.
-- ------------------------------------
checkFunctionParameters :: [Var] -> [Expr] -> Id -> SemanticAnalyzerState [Expr]
checkFunctionParameters [] _ _ = return []
checkFunctionParameters _ [] _ = return []
checkFunctionParameters (v : vs) (e : es) id = do 
  if length (v : vs) /= length (e : es) 
    then error $ "Erro: a função " ++ show id ++ "() espera " ++ show (length (v : vs)) ++ " parâmetros, mas foram dados " ++ show (length (e : es)) ++ " expressões"  
  else do
    checkedExpr <- checkExpr e
    exprType <- getExprType checkedExpr
    let (_ :#: t) = v
    case (t, exprType) of
      (TInt, TInt) -> (checkedExpr :) <$> checkFunctionParameters vs es id
      (TInt, TDouble) -> do
        warn $ "Parâmetro da função " ++ id ++ "(), do tipo Int, recebeu valor do tipo Double"
        (DoubleInt e :) <$> checkFunctionParameters vs es id
      (TDouble, TDouble) -> (checkedExpr :) <$> checkFunctionParameters vs es id
      (TDouble, TInt) -> (IntDouble checkedExpr :) <$> checkFunctionParameters vs es id
      (TString, TString) -> (checkedExpr :) <$> checkFunctionParameters vs es id
      (TVoid, TVoid) -> (checkedExpr :) <$> checkFunctionParameters vs es id
      (_, _) -> error $ "Erro: Parâmetros inválidos na função " ++ id ++ "!"

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




