import Parsing


data Expr = Num Double
           | Var 
           | Add Expr Expr 
           | Mul Expr Expr
           | Sin Expr
           | Cos Expr

x :: Expr
x = Var 

num :: Double -> Expr
num n = Num n

add :: Expr -> Expr -> Expr 
add ex1 ex2 = Add ex1 ex2 

mul :: Expr -> Expr -> Expr 
mul ex1 ex2 = Mul ex1 ex2 

sin :: Expr -> Expr
sin ex = Sin ex

cos :: Expr -> Expr
cos ex = Cos ex 

size :: Expr -> Int
size (Num n)       = 0  
size Var           = 0 
size (Sin ex)      = 1 + size ex
size (Cos ex)      = 1 + size ex
size (Add ex1 ex2) = 1 + size ex1 + size ex2
size (Mul ex1 ex2) = 1 + size ex1 + size ex2

--B
showExpr :: Expr -> String
showExpr (Num n)       = "n"
showExpr Var           = "x"
showExpr (Sin ex)      = "sin" ++ " " ++ showExpr ex
showExpr (Add ex1 ex2) = showExpr ex1 ++ "+" ++ showExpr ex2
showExpr (Mul ex1 ex2) = showFactor ex1 ++  "*" ++ showFactor ex2 

showFactor :: Expr -> String
showFactor (Add a b) = "("++showExpr (Add a b)++")"
showFactor e = showExpr e

--C
eval :: Expr -> Double -> Double
eval (Num n) varValue = n 
eval Var varValue = varValue 
eval (Sin ex) varValue = Prelude.sin (eval ex varValue)
eval (Cos ex) varValue = Prelude.cos (eval ex varValue)
eval (Add ex1 ex2) varValue =  eval ex1 varValue + eval ex2 varValue
eval (Mul ex1 ex2) varValue =  eval ex1 varValue * eval ex2 varValue

--D
readExpr :: String -> Maybe Expr
readExpr str = case parse expr str of
    Just (result, "") -> Just result -- Successfully parsed
    _                 -> Nothing    -- Parsing failed or leftover input
{- EBNF:
expr   ::= term {"+" term}.
term   ::= factor {"*" factor}.
factor ::= "cos" factor |  "sin" factor | variable |number | "(" expr ")".
-}

expr, term, factor :: Parser Expr
expr = foldl1 Add <$> chain term (char '+')

term = foldl1 Mul <$> chain factor (char '*')

factor = 
    Num <$> number 
    <|> (char 'x' *> return Var)
    <|> Sin <$> (parseSin *> factor)
    <|> Cos <$> (parseCos *> factor)
    <|> (char '(' *> expr <* char ')')

number :: Parser Double 
number = readsP

parseSin :: Parser String 
parseSin = do 
    _ <- char 's'
    _ <- char 'i'
    _ <- char 'n'
    return "sin"

parseCos :: Parser String 
parseCos = do 
    _ <- char 'c'
    _ <- char 'o'
    _ <- char 's'
    return "cos"



