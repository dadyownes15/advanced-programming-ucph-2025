module APL.Eval
  (
    eval,
    Val(..),
    envEmpty
  )
where

import APL.AST (Exp(CstInt, Add, Sub, Div, Mul, Pow, If, Eql, CstBool, Let, Var), VName,)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend name value env = (name, value) : env 


envLookup :: VName -> Env -> Maybe Val
envLookup = lookup 



data Val
  = ValInt Integer
  | ValBool Bool  
  deriving (Eq, Show)


type Error = String

evalHelper :: (Integer -> Integer -> Integer) -> Exp -> Exp -> Either Error Val

evalHelper f left right = case (eval envEmpty left, eval envEmpty right) of 
  (Right (ValInt vint1), Right (ValInt vint2)) -> Right $ ValInt $ vint1 `f` vint2
  (Right _ , Right _) -> Left "Non-integer operand" 
  (Left e, _) -> Left e
  (_, Left e) -> Left e
  
evalHelper' :: (Integer -> Integer -> Either Error Integer) -> Exp -> Exp -> Either Error Val
evalHelper' f left right = case (eval envEmpty left, eval envEmpty right) of 
  (Right (ValInt vint1), Right (ValInt vint2)) -> case f vint1 vint2 of
      Left err -> Left err
      Right z -> Right $ ValInt z
  (Right _ , Right _) -> Left "Non-integer operand" 
  (Left e, _) -> Left e
  (_, Left e) -> Left e

eval :: Env -> Exp -> Either Error Val
eval env (CstInt int) = Right $ ValInt int
eval env (CstBool bool) = Right $ ValBool bool
eval env (Add left right) = evalHelper (+) left right
eval env (Sub left right) = evalHelper (-) left right
eval env (Mul left right) = evalHelper (*) left right 
eval env (Div e1 e2) = evalHelper' checkedDiv e1 e2
  where
    checkedDiv _ 0 = Left "Division by zero"
    checkedDiv x y = Right $ x `div` y
eval env (Pow e1 e2) = evalHelper' checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then Left "Negative exponent"
        else Right $ x ^ y
eval env (Eql left right) = case (eval envEmpty left, eval envEmpty right) of 
  (Right (ValInt vint1), Right (ValInt vint2)) -> Right $ ValBool $ vint1 == vint2 
  (Right (ValBool bool1), Right (ValBool bool2)) -> Right $ ValBool $ bool1 == bool2
  (Right (ValBool _bool1), Right(ValInt _int1)) -> Left "Cannot combine the bool1 and in1"  
  (Right(ValInt _int1), Right (ValBool _bool1)) -> Left "Cannot combine the bool1 and in1"  
  (Left e, _) -> Left e
  (_, Left e) -> Left e  
eval env (If exp1 exp2 exp3) = case eval envEmpty exp1 of 
  (Right (ValBool True)) -> eval envEmpty exp2 
  (Right (ValBool False)) -> eval envEmpty exp3 
  (Right _) -> Left "Cannot use expressions of ints"
  (Left e) -> Left e         
eval env (Let name exp1 exp2) = case eval env exp1 of 
  Right val -> eval (envExtend name val env) exp2 
  Left err -> Left err
eval env (Var name) = case envLookup name env of 
  Just val -> Right val
  Nothing -> Left "No variable defined"

{-
1.
eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "x")))

2.
eval ("x", (CstInt 3)) (Add (Var "x") (Var "y")) 

3. 

eval ("x", (CstInt 3)) (Add (eval(Var "x")) (eval (Var "x"))) 

4. 

eval ("x", (CstInt 3)) (Add ValInt 3 Valint 3) 

eval ("x", (CstInt 3)) (Add ValInt 3 Valint 3)  -> Right ValInt 6
-}


