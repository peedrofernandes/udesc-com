import Text.Parsec

partida :: Parsec String u Integer
partida = expr >>= \e ->
          eof >>
          return e

expr = spaces >> expr'

expr' = (char '+' >> expr >>= \e1 -> expr >>= \e2 -> return (e1 + e2)) <|>
        (char '-' >> expr >>= \e1 -> expr >>= \e2 -> return (e1 - e2)) <|>
        (char '*' >> expr >>= \e1 -> expr >>= \e2 -> return (e1 * e2)) <|>
        (char '/' >> expr >>= \e1 -> expr >>= \e2 -> return (div e1 e2)) <|>
        constante
    
constante = many1 digit >>= \d ->
            return (read d)

parserEP e = runParser partida [] "Expressões pré-fixadas " e

parserExpr s = case parserEP s of
                Left er -> print er
                Right v -> (print "Resultado: " >> print v)

main = putStr "Digite uma expressão em notação pré-fixada: " >>
        getLine >>= \e ->
        parserExpr e

