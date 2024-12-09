import Parsing
import Data.Char
import Test.QuickCheck


data Expr = Num Double
           | Var 
           | Add Expr Expr 
           | Mul Expr Expr
           | Sin Expr
           | Cos Expr
           deriving Eq

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
showExpr (Num n)       = show n
showExpr Var           = "x"
showExpr (Sin ex)      = "sin" ++ case ex of 
    Mul _ _    ->  " " ++ "(" ++ showExpr ex ++ ")"
    Add _ _    ->  " " ++ "(" ++ showExpr ex ++ ")"
    _          ->  " " ++ showExpr ex
showExpr (Cos ex)      = "cos" ++ case ex of 
    Mul _ _    ->  " " ++ "(" ++ showExpr ex ++ ")"
    Add _ _    ->  " " ++ "(" ++ showExpr ex ++ ")"
    _          ->  " " ++ showExpr ex
showExpr (Add ex1 ex2) = showExpr ex1 ++ "+" ++ showExpr ex2
showExpr (Mul ex1 ex2) = showFactor ex1 ++  "*" ++ showFactor ex2 

instance Show Expr where 
    show = showExpr

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

ex :: Expr 
ex = Sin (Add (Num 3.2) Var) 

--D
readExpr :: String -> Maybe Expr
readExpr str = case parse expr (removeSpaces str) of
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
    <|> Sin <$> (char 's' *> char 'i' *> char 'n' *> factor)
    <|> Cos <$> (char 'c' *> char 'o' *> char 's' *> factor)
    <|> (char '(' *> expr <* char ')')

number :: Parser Double 
number = readsP

removeSpaces :: String -> String
removeSpaces = filter (not . isSpace)

assoc :: Expr -> Expr
assoc (Add (Add e1 e2) e3) = assoc (Add e1 (Add e2 e3)) 
assoc (Add e1 e2)          = Add (assoc e1) (assoc e2)
assoc (Mul (Mul e1 e2) e3) = assoc (Mul e1 (Mul e2 e3)) 
assoc (Mul e1 e2)          = Mul (assoc e1) (assoc e2)
assoc (Sin ex)             = Sin (assoc ex)
assoc (Cos ex)             = Cos (assoc ex)
assoc e                    = e
        

--E
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr ex = 
    let showResult = showExpr ex
        e          = readExpr showResult 
    in case e of 
         Nothing     -> False
         Just result -> assoc result == assoc ex

arbExpr :: Int -> Gen Expr
arbExpr s = 
    frequency [ (1, do n <- arbitrary
                       return (Num n)),
                (s, do a <- arbExpr s'
                       b <- arbExpr s'
                       return (Add a b)),
                (s, do a <- arbExpr s'
                       b <- arbExpr s'
                       return (Mul a b)),
                (1,return Var),
                (s, do a <- arbExpr s'
                       return (Sin a)),
                (s, do a <- arbExpr s'
                       return (Cos a))]
    where s' = s `div` 2


instance Arbitrary Expr where 
  arbitrary = sized arbExpr

--F
simplify :: Expr -> Expr
simplify (Add (Num a) (Num b)) = Num $ a + b
simplify (Add Var (Num 0))   = Var
simplify (Add (Num 0) Var)   = Var 
simplify (Mul (Num a) (Num b)) = Num $ a * b
simplify (Mul Var (Num 0))   = Num 0
simplify (Mul (Num 0) Var)   = Num 0 
simplify (Mul Var (Num 1))   = Var
simplify (Mul (Num 1) Var)   = Var
simplify e                   = e

prop_simplify :: Expr -> Double -> Bool 
prop_simplify ex n = eval ex n == eval (simplify ex) n 

--G
differentiate :: Expr -> Expr
differentiate Var = Num 1 
differentiate Num = 0 
differentiate Add ex1 ex2 = ((differentiate ex1) * ex2) + ((differentiate ex2) * ex1)
differentiate Sin ex = Cos ex 
differentiate Cos ex = -Sin ex
differentiate Mul ex1 ex2 = 



