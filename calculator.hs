import System.IO
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

operators = emptyDef { opStart = oneOf "+-*/"
               , opLetter = oneOf "+-/*"
               }

lexer :: TokenParser()
lexer = makeTokenParser operators

binary name fun assoc = Infix (do{ reservedOp lexer name; return fun }) assoc

parseTable = [
               [ binary "*" (*) AssocLeft,
               binary "/" (/) AssocLeft ]
               , [ binary "+" (+) AssocLeft ,
               binary "-" (-) AssocLeft ]
             ]



parseNum :: Parser Double
parseNum = do
  val <- naturalOrFloat lexer
  case val of
    Left int -> return $ fromInteger int
    Right nat -> return $ nat


parseExpr :: Parser Double
parseExpr = buildExpressionParser parseTable parseTerm


parseTerm :: Parser Double
parseTerm = parens lexer parseExpr <|> parseNum

doCalc :: String -> String
doCalc s =
  case ret of
      Left e -> "error: " ++ (show e)
      Right n -> "result = " ++ (show n)
  where
      ret = parse parseExpr "" s


checkParen :: String -> Bool
checkParen s
  | head s == '(' && last s == ')' = True
  | otherwise = False

main :: IO()
main = do putStr("calc >> ")
          hFlush stdout
          line <- getLine
          -- force user to surround expression with parenthesis
          --
          if (checkParen line )then do putStrLn( doCalc line)
          else do putStrLn ("Error: Must Surround Expr with Parens")
          main
