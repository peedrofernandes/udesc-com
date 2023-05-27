module Main where

import Text.Parsec
import Parser

start input = runParser parserPrograma [] "Expressões" input

compile s = case start s of
  Left error -> print error
  Right arvoreSintatica -> putStrLn ("Compilado com sucesso - Arvore sintatica do programa: " ++ show arvoreSintatica)

main = do { code <- readFile "teste1.txt" 
          ; compile code }

