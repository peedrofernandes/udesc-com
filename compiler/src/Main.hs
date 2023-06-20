module Main where

import Text.Parsec ( runParser, ParseError )
import Parser
import CompilerProps
import System.IO
import qualified Data.Map as Map
import SemanticAnalyzer
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Identity (Identity(runIdentity))
import GHC.IO (unsafePerformIO)

-- parsePrograma :: String -> Either ParseError Programa
-- parsePrograma = runParser parserPrograma [] "Expressões"

-- parseExpr :: String -> Either ParseError Expr
-- parseExpr = runParser parserExpressao [] "Expr"

parsePrograma :: IO ()
parsePrograma = do
  str <- readFile "./code/teste1.txt"
  case runParser parserPrograma [] "Programa" str of
    Left error -> print error
    Right arvoreSintatica -> do writeFile "./output/programa.txt" (show arvoreSintatica)

compile :: IO ()
compile = do
  str <- readFile "./code/teste1.txt"
  case runParser parserPrograma [] "Programa" str of
    Left error -> print error
    Right arvoreSintatica -> do 
      writeFile "./output/programa.txt" (show arvoreSintatica)
      runSemanticAnalyzer arvoreSintatica
  
-- parseExpr :: IO ()
-- parseExpr = do
--   str <- readFile "./code/expr.txt"
--   case runParser parserExpressao [] "Expressões" str of
--     Left error -> print error
--     Right arvoreSintatica -> do 
--       writeFile "./output/expr.txt" (show arvoreSintatica ++ "\n\n")
--       appendFile "./output/expr.txt" (show (semanticExpr arvoreSintatica))

-- runSemanticAnalyzer :: SemanticAnalyzerState a -> IO a
-- runSemanticAnalyzer state = do 
--   (result, (_, warnings)) <- runStateT state (Map.empty, [])
--   mapM_ putStrLn warnings
--   return result

runSemanticAnalyzer :: Programa -> IO ()
runSemanticAnalyzer programa = do
  let estadoInicial = ("", Map.empty, [])
  (prog, (scope, table, warnings)) <- runStateT (checkPrograma programa) estadoInicial
  writeFile "./output/programaAnalisado.txt" (show prog)
  mapM_ putStrLn warnings
  putStrLn "Compilado com sucesso."

-- runSemanticAnalyzer :: IO ()
-- runSemanticAnalyzer = do
--   str <- readFile "./output/programa.txt"
--   case runSemanticAnalyzer 


-- semanticExpr :: Programa -> Programa
-- semanticExpr expr = unsafePerformIO $ runSemanticAnalyzer $ checkPrograma 

main :: IO ()
main = do
  compile