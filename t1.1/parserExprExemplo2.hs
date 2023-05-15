import Text.Parsec.Language
import Text.Parsec.Token as T

tokens = emptyDef {
  T.commentStart = "{-",
  T.commentEnd = "-}",
  T.commentLine = "--",
  T.reservedOpNames = ["+", "-", "/", "*"]
}

lexer = T.makeTokenParser tokens
natural = T.natural lexer
symbol = T.symbol lexer
parens = T.parens lexer
reservedOp = T.reservedOp lexer
