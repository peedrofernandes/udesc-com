-- Gramática da linguagem
{-

S -> E#
E -> TE'
E' -> +TE'
   | -TE'
   | vazio
T -> FT'
T' -> *FT'
   | /FT'
   | vazio
F -> (E)
  | c

Recognizing 1 + 2 * 3 = c + c * c

S -> E -> T E' -> F T' E' -> c T' E' -> c E' -> c + T E' -> c + F T' E' -> c + c T' E' -> c + c * F T' E' -> c + c * c T' E' -> c + c * c E' -> c + c * c

-}

import Text.Parsec

type Parser u r = Parsec String u r

parser :: Parser u Integer
parser = do {e <- parserE; eof; return e};

parserE :: Parser u Integer
parserE = do {t <- parserT; e' <- parserE'; return (e' t)}

parserE' :: Parser u (Integer -> Integer)
parserE' = do {char '+'; t <- parserT; e' <- parserE'; return ((e' . (+t)))}
           <|> do {char '-'; t <- parserT'; e' <- parserE'; return (e' . (-t))}
           <|> do {return id}

parserT :: Parser u Integer
parserT = do {f <- parserF; t' <- parserT'; return (t' f)}

parserT' :: Parser u (Integer -> Integer)
parserT' = do {char '*'; f <- parserF; t' <- parserT'; return (t' . (*f))}
           <|> do {char '/'; f <- parserF; t' <- parserT'; return (t' . (/f))}
           <|> do {return id}

parserF :: Parser u Integer
parserF = do {char '('; e <- parserE; char ')'; return e}
          <|> do {c <- parserCons; return c}

parserCons :: Parser u Integer
parserCons = do {c <- many1 digit; return (read c)};

start e = runParser parser [] "Expressões " e

parserExpr s = case start s of 
                Left er -> print er
                Right v -> print v

main = do {
  putStr "Expressão: ";
  e <- getLine;
  parserExpr e
}

