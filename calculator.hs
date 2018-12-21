import System.IO
import System.Exit
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

--Sources:
--http://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-ParserCombinators-Parsec-Token.html
--https://commentedcode.org/blog/2017/05/21/haskell-project-intro-to-parsers/
--https://kseo.github.io/posts/2014-01-07-parsing-arithmetic-expressions-with-parsec.html

-- Parse table from Text.Parsec.Expr documentation:
-- http://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-Parsec-Expr.html

operators = emptyDef { reservedOpNames = ["+", "-", "*", "/"] }


lexer :: TokenParser()
lexer = makeTokenParser operators


binary name fun assoc = Infix (do{ reservedOp lexer name; return fun }) assoc
prefix name fun = Prefix (do{ reservedOp lexer name; return fun})

parseTable = [ [ prefix "-" negate],
               [ binary "*" (*) AssocLeft,
               binary "/" (/) AssocLeft ]
               , [ binary "+" (+) AssocLeft ,
               binary "-" (-) AssocLeft ]
             ]


parseNum :: Parser Double
parseNum = do
  result <- naturalOrFloat lexer
  case result of
    Left int -> return $ fromInteger int
    Right flt -> return $ flt


parseExpr :: Parser Double
parseExpr = buildExpressionParser parseTable parseTerm


parseTerm :: Parser Double
parseTerm = parens lexer parseExpr <|> parseNum

doCalc :: String -> String
doCalc s =
  case ret of
      Left err -> "error: " ++ (show err)
      Right num -> "result = " ++ (show num)
  where
      ret = parse parseExpr "" s


checkParen :: String -> Bool
checkParen s
  | head s == '(' && last s == ')' = True
  | otherwise = False

checkQuit :: String -> Bool
checkQuit s
   | head s == 'q' = True
   | otherwise = False

main :: IO()
main = do putStr("calc >> ")
          hFlush stdout
          line <- getLine
          -- force user to surround expression with parenthesis
          --
          if (checkQuit line)
            then return()
          else do
            if (checkParen line )
              then putStrLn( doCalc line)
            else putStrLn ("Error: Must Surround Expr with Parens")
            main
