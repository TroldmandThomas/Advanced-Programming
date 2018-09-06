
{-# OPTIONS_GHC -W #-}

import Definitions


-- We will build up a string to represent the expression as a mathematical equation
-- If we encounter a number, we simply add that number to the string we build.
-- When we encounter an operator, we insert the correct operator symbol, 
-- then recursively call the equations on either side, and enclose it all with
-- parentheses on either side.
showExp :: Exp -> String
showExp (Cst c)     = show c
showExp (Add e1 e2) = "(" ++ showExp e1 ++ "+" ++ showExp e2 ++ ")"
showExp (Sub e1 e2) = "(" ++ showExp e1 ++ "-" ++ showExp e2 ++ ")"
showExp (Mul e1 e2) = "(" ++ showExp e1 ++ "*" ++ showExp e2 ++ ")"
showExp (Div e1 e2) = "(" ++ showExp e1 ++ "/" ++ showExp e2 ++ ")"
showExp (Pow e1 e2) = "(" ++ showExp e1 ++ "^" ++ showExp e2 ++ ")"
showExp _           = error "Functionality for 'If, Var, Let and Sum' \
                             \are not supported by showExp"

-- If we encounter a number, simply return that as the value of the expression.
-- When an opreator is encountered, we evaluate it with the appropriate operator,
-- and recursively call the rest of the expression. 
-- Constraints for power operator is implemented as designated in assignment text
-- (n^0 = 1 for all n >= 0)
-- floor division is implemented with `div`, since it rounds greatest integer 
-- less than or equal to x, as opposed do Prelude.floor, which rounds to nearest integer.
evalSimple :: Exp -> Integer
evalSimple (Cst c)                 = c
evalSimple (Add e1 e2)             = (evalSimple e1) + (evalSimple e2)
evalSimple (Sub e1 e2)             = (evalSimple e1) - (evalSimple e2)
evalSimple (Mul e1 e2)             = (evalSimple e1) * (evalSimple e2)
evalSimple (Div e1 e2)             = (evalSimple e1) `div` (evalSimple e2)
evalSimple (Pow _ (Cst 0))         = 1 :: Integer                          --maybe redundant case
evalSimple (Pow _ (Cst c)) | c < 0 = error "Power must be non-negative"
evalSimple (Pow e1 e2)             = (evalSimple e1) ^ (evalSimple e2)
evalSimple _                       = error "Functionality for 'If, Var, Let and Sum' \
                                      \are not supported by evalExp"

--evalSimple e = read ("\"" ++ (showExp e) ++ "\"")


--extendEnv :: VName -> Integer -> Env -> Env
--extendEnv var val env = let (\var -> (Just val)) in env 



evalFull :: Exp -> Env -> Integer
evalFull (Cst c) _                                       = c
evalFull (Add e1 e2) env                                 = (evalFull e1 env) + (evalFull e2 env)
evalFull (Sub e1 e2) env                                 = (evalFull e1 env) - (evalFull e2 env)
evalFull (Mul e1 e2) env                                 = (evalFull e1 env) * (evalFull e2 env)
evalFull (Div e1 e2) env                                 = (evalFull e1 env) `div` (evalFull e2 env)
evalFull (Pow _ (Cst 0)) _                               = 1 :: Integer          --maybe redundant case
evalFull (Pow _ (Cst c)) _ | c < 0                       = error "Power must be non-negative"
evalFull (Pow e1 e2) env                                 = (evalFull e1 env) ^ (evalFull e2 env)
evalFull (If e1 _ e3) env | ((evalFull e1 env) == 0)     = evalFull e3 env
evalFull (If  _ e2 _) env                                = evalFull e2 env
evalFull (Var var) env                                   =  case env var of
                                                             Just n -> n
                                                             Nothing -> error "Variable was not in environment" 
evalFull (Let var aux body) env                          = let aux' = evalFull aux env 
                                                               env' = extendEnv var aux' env
                                                            in evalFull body env'   
--evalFull (Add e1 e2)                                = (evalFull e1) + (evalFull e2) 
evalFull (Sum var from to body) env                   = let n1 = evalFull from env
                                                            n2 = evalFull to env
                                                            env' = extendEnv var n1 env
                                                        in if n1 > n2
                                                                then 0 :: Integer
                                                                else evalFull body env' + 
                                                                     evalFull (Sum var (Cst (n1+1)) to body) env
evalFull _ _                                             = error "Not implemented yet"
        
 
testIf  = show (evalFull (If (Sub (Cst 2) (Cst 2)) (Div (Cst 3) (Cst 0)) (Cst 5)) initEnv)
testIf2 = show (evalFull 
               (If (Sub (Cst 2) (Cst 3)) (Div (Cst 3) 
                (Cst 3)) (Div (Cst 4) (Cst 0))) initEnv)


extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r = \e -> if (e == v) 
                           then Just n
                           else r e


r' = extendEnv "x" 55 initEnv
r'' = extendEnv "x" 2 r'

vartest = show $ evalFull (Var "x") r'
vartest2 = show $ evalFull (Add (Var "x") (Var "x")) r'
vartest3 = show $ evalFull (Add (Var "x") (Var "x")) r''
vartest4 = show $ evalFull (Add (Var "y") (Var "x")) r''


lettest = show $ evalFull (Let {var = "x", aux = Cst 5,
                               body = Add (Let {var = "x", aux = Add (Cst 3) (Cst 4),
                               body = Mul (Var "x") (Var "x")})
                               (Var "x")} ) initEnv
--error case
--Evaluate to 5, since we simulate lazy evaluation. 
--lazy evaluation seems natural, since we use Haskell.
lettest2 = show $ evalFull (Let "x" (Div (Cst 4) (Cst 0)) (Cst 5)) initEnv



sumtest = show $ evalFull (Sum {var = "i", from = (Cst 0), to = (Cst 3), 
                                body = (Add (Var "i") (Cst 0))}) initEnv
    



--TEST FOR SHOWEXP
--Add test
a = showExp (Add (Add (Cst 3) (Cst 4)) (Cst 2))
a1 = show ("((3+4)+2)" == a)
--Sub test
b = showExp (Sub (Cst 5) (Cst 9))
b1 = show ("(5-9)" == b)
--Mul test
c = showExp (Add (Mul (Cst 2) (Cst 3)) (Cst 4))
c1 = show ("((2*3)+4)" == c)
--Div Test
d = showExp (Div (Cst 8) (Cst 4))
d1 = show ("(8/4)" == d)
--Pow Test
e = showExp (Pow (Cst 2) (Cst 8))
e1 = show ("(2^8)" == e)
--All test
f = showExp (Div (Add (Mul (Cst 4) (Cst 5)) (Sub (Cst 2) (Cst 1))) (Pow (Cst 3) (Cst 7)))
f1 = show ("(((4*5)+(2-1))/(3^7))" == f)
--Error test
--g = showExp (Var "Hej")


--TEST for evalSimple
h = show (evalSimple (Add (Add (Cst 3) (Cst 4)) (Cst 2)))
i = show (evalSimple (Sub (Cst 5) (Cst 9)))
j = show (evalSimple (Add (Mul (Cst 2) (Cst 3)) (Cst 4)))
k = show (evalSimple (Div (Cst 8) (Cst 4)))
l = show (evalSimple (Div (Cst 7) (Cst 2)))
m = show (evalSimple (Div (Cst 7) (Cst (-2))))
--n = show (evalSimple (Div (Cst 7) (Cst (0))))
o = show (evalSimple ((Pow (Cst 2) (Cst 8))))


main = do 
    putStrLn a1
    putStrLn b1
    putStrLn c1
    putStrLn d1
    putStrLn e1
    putStrLn f1
  --  putStrLn g
    putStrLn h
    putStrLn i
    putStrLn j
    putStrLn k
    putStrLn l
    putStrLn m
  --  putStrLn n
    putStrLn o
    putStrLn testIf
    putStrLn testIf2
    putStrLn ""
    putStrLn vartest
    putStrLn vartest2
    putStrLn vartest3
  --  putStrLn vartest4
    putStrLn lettest
    putStrLn lettest2
    putStrLn sumtest







