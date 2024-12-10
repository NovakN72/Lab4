module Expr where
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
simplify ex = 
    let simEx = simplifyHelper ex 
    in if simEx == ex
       then simEx 
    else simplify simEx


simplifyHelper :: Expr -> Expr
simplifyHelper (Num n) = Num n
simplifyHelper Var = Var

simplifyHelper (Add (Num a) (Num b)) = Num $ a + b
simplifyHelper (Add ex (Num 0))   = simplify ex
simplifyHelper (Add (Num 0) ex)   = simplify ex 
simplifyHelper (Add ex1 ex2) = Add (simplify ex1) (simplify ex2)


simplifyHelper (Mul (Num a) (Num b)) = Num $ a * b
simplifyHelper (Mul ex (Num 0))   = Num 0
simplifyHelper (Mul (Num 0) ex)   = Num 0 
simplifyHelper (Mul ex (Num 1))   = simplify ex
simplifyHelper (Mul (Num 1) ex)   = simplify ex
simplifyHelper (Mul ex1 ex2) = Mul (simplify ex1) (simplify ex2)

simplifyHelper (Sin ex) = Sin (simplify ex)
simplifyHelper (Cos ex) = Cos (simplify ex)
simplifyHelper (Sin (Num n)) = Num (Prelude.sin n)
simplifyHelper (Sin (Num n)) = Num (Prelude.cos n)



prop_simplify :: Expr -> Double -> Bool 
prop_simplify ex n = eval ex n == eval (simplify ex) n 

--G
differentiate :: Expr -> Expr
differentiate Var         = Num 1 
differentiate (Num n)         = Num 0 
differentiate (Add ex1 ex2) = simplify $ Add (differentiate ex1) (differentiate ex2)
differentiate (Sin ex)      = simplify $ Mul (differentiate ex) (Cos ex) 
differentiate (Cos ex)      = simplify $ Mul (differentiate ex) (Mul(Num (-1)) (Sin ex))
differentiate (Mul ex1 ex2) = simplify $ Add (Mul (differentiate ex1) ex2) (Mul ex1 (differentiate ex2))

