module APL.AST
  (Exp(CstInt, Add, Sub, Mul, Div, Pow, CstBool, Eql, If, Let, Var), VName)
where

data Exp
  = CstInt Integer
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | CstBool Bool
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp

  deriving (Eq, Show)

type VName = String