-- module Main where

import Text.Parsec ( runParser, ParseError )
import Parser
import CompilerProps
import System.IO
import qualified Data.Map as Map
import SemanticAnalyzer
import Control.Monad.State
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

parseExpr :: IO ()
parseExpr = do
  str <- readFile "./code/expr.txt"
  case runParser parserExpressao [] "Expressões" str of
    Left error -> print error
    Right arvoreSintatica -> do 
      writeFile "./output/expr.txt" (show arvoreSintatica ++ "\n\n")
      appendFile "./output/expr.txt" (show (semanticExpr arvoreSintatica))

runSymbolTableState :: SymbolTableState a -> IO a
runSymbolTableState st = do 
  (result, (_, warnings)) <- runStateT st (Map.empty, [])
  mapM_ putStrLn warnings
  return result

semanticExpr :: Expr -> Expr
semanticExpr expr = unsafePerformIO $ runSymbolTableState $ checkExpr expr
