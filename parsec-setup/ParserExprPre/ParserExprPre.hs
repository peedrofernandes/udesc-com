import Text.Parsec

-- Inicializa o encadeamento de parsers
partida :: Parsec String userState Integer
partida = do {e <- expr; eof; return e}

-- Esse parser ignora todos os espaços
expr = do {spaces; expr'}

expr' = do {char '+'; e1 <- expr; e2 <- expr; return (e1 + e2)}
        <|> do {char '-'; e1 <- expr; e2 <- expr; return (e1 - e2)}
        <|> do {char '*'; e1 <- expr; e2 <- expr; return (e1 * e2)}
        <|> do {char '/'; e1 <- expr; e2 <- expr; return (div e1 e2)}
        <|> do {constante}

constante = do {d <- many digit; return (read d)}

parserEP e = runParser partida [] "Expressões pré-fixadas " e

parserExpr s = case parserEP s of 
                Left er -> print er
                Right v -> (print "resultado" >> print v)
    
main = do putStr "Expressão: "
          e <- getLine
          parserExpr e