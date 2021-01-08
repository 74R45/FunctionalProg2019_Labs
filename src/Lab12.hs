{-# OPTIONS_GHC -Wall #-}
module Lab12 where

--------------------
-- Interpretation --
--------------------

data Value = I Int | B Bool deriving (Show, Eq)
data Exp = Var String      -- variable
         | Const Value     -- constant
         | Op Exp Bop Exp  -- operation
                 deriving (Show, Eq)
-- Binary arguments
data Bop =  Plus | Minus | Times | Div
          | Gt | Ge | Lt | Le | Eql | And | Or
            deriving (Show, Eq)

data Stmt = Assign String Exp
          | Incr String
          | If Exp Stmt Stmt
          | While Exp Stmt
          | For Stmt Exp Stmt Stmt
          | Block [(String,Type)] [Stmt]
          deriving (Show, Eq)
data Type = It | Bt deriving (Show, Eq)
type Program = Stmt

type StateW = [(String, Value)]

type VarEnv  = [(String,Type)]

-- Task 1 -----------------------------------------
getValue::  StateW -> String -> Value
getValue [] _ = error "The value couldn't be found."
getValue ((var0,val):rest) var | var0 == var = val
                               | otherwise = getValue rest var

updValue :: StateW -> String -> Value -> StateW
updValue [] _ _ = error "The value couldn't be found."
updValue ((var0,val0):rest) var val | var0 == var = (var,val):rest
                                    | otherwise = (var0,val0) : updValue rest var val

-- Task 2 -----------------------------------------
evExp :: StateW -> Exp -> Value
evExp _ (Const v) = v
evExp st (Var v) = getValue st v
evExp st (Op x0 op y0) = case (op, evExp st x0, evExp st y0) of
  (Plus, I x, I y) -> I (x + y)
  (Minus, I x, I y) -> I (x - y)
  (Times, I x, I y) -> I (x * y)
  (Div, I x, I y) -> I (x `quot` y)
  (Gt, I x, I y) -> B (x > y)
  (Ge, I x, I y) -> B (x >= y)
  (Lt, I x, I y) -> B (x < y)
  (Le, I x, I y) -> B (x <= y)
  (Eql, I x, I y) -> B (x == y)
  (And, B x, B y) -> B (x && y)
  (Or, B x, B y) -> B (x || y)
  _ -> error "Incorrect types."

-- Task 3 -----------------------------------------
evStmt :: StateW -> Stmt -> StateW
evStmt st (Assign x val) = updValue st x (evExp st val)
evStmt st (Incr x) = updValue st x $ I (getInt (getValue st x) + 1)
evStmt st (If expr stmt1 stmt2) | getBool $ evExp st expr = evStmt st stmt1
                                | otherwise = evStmt st stmt2
evStmt st (While expr stmt) = until (\st1 -> not $ getBool $ evExp st1 expr)
                                    (\st1 -> evStmt st1 stmt) st
evStmt st (For initStmt expr updStmt bodyStmt) = until (\st1 -> not $ getBool $ evExp st1 expr)
                                                       (\st1 -> evStmt (evStmt st1 bodyStmt) updStmt)
                                                       (evStmt st initStmt)
evStmt st (Block inits stmts) = case inits of
  ((x, It):rest) -> tail $ evStmt ((x, I 0):st) (Block rest stmts)
  ((x, Bt):rest) -> tail $ evStmt ((x, B False):st) (Block rest stmts)
  [] -> case stmts of
    (stmt:restStmts) -> evStmt (evStmt st stmt) (Block [] restStmts)
    [] -> st

getBool :: Value -> Bool
getBool (B b) = b
getBool (I _) = error "Couldn't match type \"Bool\" with type \"Int\"."

getInt :: Value -> Int
getInt (I x) = x
getInt (B _) = error "Couldn't match type \"Int\" with type \"Bool\"."

-- Task 4 -----------------------------------------
evProgram :: Program -> StateW
evProgram (Block inits stmts) = evStmt (initialize inits) (Block [] stmts)
  where initialize :: [(String,Type)] -> [(String,Value)]
        initialize [] = []
        initialize ((x,It):rest) = (x, I 0):(initialize rest)
        initialize ((x,Bt):rest) = (x, B False):(initialize rest)
evProgram _ = error "Program is not a block."

-- Task 5 -----------------------------------------
iswfOp :: Bop -> [Type] -> Maybe Type
iswfOp Plus [It,It] = Just It
iswfOp Minus [It,It] = Just It
iswfOp Times [It,It] = Just It
iswfOp Div [It,It] = Just It
iswfOp Gt [It,It] = Just Bt
iswfOp Ge [It,It] = Just Bt
iswfOp Lt [It,It] = Just Bt
iswfOp Le [It,It] = Just Bt
iswfOp Eql [It,It] = Just Bt
iswfOp And [Bt,Bt] = Just Bt
iswfOp Or [Bt,Bt] = Just Bt
iswfOp _ _ = Nothing

-- Task 6 -----------------------------------------
iswfExp :: Exp -> VarEnv -> Maybe Type
iswfExp (Const (I _)) _ = Just It
iswfExp (Const (B _)) _ = Just Bt
iswfExp (Var _) [] = Nothing
iswfExp (Var x) ((v,t):ve) | x == v = Just t
                           | otherwise = iswfExp (Var x) ve
iswfExp (Op x op y) ve = case (iswfExp x ve, iswfExp y ve) of
  (Just t1, Just t2) -> iswfOp op [t1, t2]
  _ -> Nothing

-- Task 7 -----------------------------------------
iswfStmt :: Stmt -> VarEnv -> Bool
iswfStmt (Assign x expr) ve = case iswfExp expr ve of
  Just t -> checkType ve (x,t)
  Nothing -> False
iswfStmt (Incr x) ve = checkType ve (x,It)
iswfStmt (If expr stmt1 stmt2) ve = iswfExp expr ve == Just Bt &&
                                    iswfStmt stmt1 ve &&
                                    iswfStmt stmt2 ve
iswfStmt (While expr stmt) ve = iswfExp expr ve == Just Bt && iswfStmt stmt ve
iswfStmt (For (Assign v val) expr updStmt bodyStmt) ve1 = case iswfExp val ve1 of
  Nothing -> False
  Just t -> let ve2 = (v,t):ve1 in
              iswfExp expr ve2 == Just Bt && iswfStmt updStmt ve2 && iswfStmt bodyStmt ve2
iswfStmt (Block inits stmts) ve = and $ map (`iswfStmt` (inits++ve)) stmts
iswfStmt _ _ = False

checkType :: VarEnv -> (String,Type) -> Bool
checkType [] _ = False
checkType (x:xs) y | x == y = True
                   | otherwise = checkType xs y

-- Main Function ----------------------------------
iswfProgram :: Program -> Bool
iswfProgram st@(Block _ _) = iswfStmt st []
iswfProgram _              = False

-- Programs ---------------------------------------

{- Calculating value b (6) to the power of e (5) into variable out

   { int b; int e; int out;
     b := 6; e := 5; out:= 1;
	 { int i; 
       for (i:=0; i<e; i++) out := out*b
	 }
   }
-}
power :: Program
power = Block [("b",It),("e",It),("out",It)]
              [Assign "b" (Const (I 6)), Assign "e" (Const (I 5)), Assign "out" (Const(I 1)),
               Block [("i",It)] 
                     [For (Assign "i" (Const(I 0))) (Op (Var "i") Lt (Var "e")) (Incr "i")
                           (Assign "out" (Op (Var "out") Times (Var "b")))
                     ]
              ] 

{- Calcuating integer part of a square root
   the value of variable a (317) into variable b

   {int a; int b;
    a := 317; b := 0;
	{bool c; c:=true;
	         while(c) {b++; c:= a >= b*b}
	};
    b := b-1
   }
-}
squareRoot :: Program
squareRoot = Block [("a",It),("b",It)]
                   [ Assign "a" (Const (I 317)), Assign "b" (Const (I 0)),
                     Block [("c", Bt)]
                            [Assign "c" (Const (B True)),
                             While (Var "c")
                                 (Block []
                                   [(Incr "b"),
                                     Assign "c" (Op (Var "a") Ge (Op (Var "b") Times (Var "b")))
                                   ])
                           ],
                     Assign "b" (Op (Var "b") Minus (Const (I 1)))
                   ]

{- Calculating 12th Fibonacci number into variable out

  {int in; int out; in := 121;
     {int f0; int f1; int c; f0 := 1; f1 := 1;
      if(in == 0) then out := f0 else
      if (in == 1) then out := f1 else 
        for (c := 2; c <= in; c++) {
         out := f0 + f1; f0 := f1; f1 := out
        }
	 }
  }
-}
fibonacci :: Program
fibonacci = Block [("in",It), ("out",It)]
                  [Assign "in" (Const (I 12)),
                   Block [("f0",It), ("f1",It), ("c",It)]
                          [Assign "f0" (Const (I 1)), Assign "f1" (Const (I 1)),
                           If (Op (Var "in") Eql (Const (I 0)))
                              (Assign "out" (Var "f0"))
                              (If (Op (Var "in") Eql (Const (I 1)))
                                  (Assign "out" (Var "f1"))
                                  (For (Assign "c" (Const (I 1)))
                                       (Op (Var "c") Lt (Var "in")) (Incr "c")
                                       (Block []
                                             [ Assign "out" (Op (Var "f0") Plus (Var "f1"))
                                             , Assign "f0" (Var "f1")
                                              , Assign "f1" (Var "out")
                                             ])
                                  )
                              )
                          ]
                  ]
