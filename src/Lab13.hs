{-# OPTIONS_GHC -Wall #-}
module Lab13 where

----------------------------------
-- Writing Parsers using Parsec --
----------------------------------

import Text.ParserCombinators.Parsec

-- Task 1 -----------------------------------------
fullBrace :: Parser()
fullBrace = try (do
  spaces
  brace
  eof
  return ())

brace :: Parser()
brace = (do
  opening <- oneOf "([{"
  spaces
  brace
  _ <- char (getClosing opening)
  spaces
  brace
  return ()) <|> spaces

getClosing :: Char -> Char
getClosing c = case c of
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  _ -> error ("Invalid input")

balance  :: String -> Bool
balance str = either (const False) (const True) (parse fullBrace "" str)
   
-- Task 2 -----------------------------------------
data Bexp = Bvalue Bool | Bvar Char | Not Bexp
          | And Bexp Bexp | Or Bexp Bexp  deriving (Eq, Show)

fullBe :: Parser Bexp
fullBe = try (do
  ex <- bexp
  eof
  return ex)

bdis :: Parser Bexp
bdis = (do
  _ <- char '('
  ex <- bexp
  _ <- char ')'
  return ex) <|> (do
  _ <- char '!'
  ex <- bdis
  return $ Not ex) <|> (do
  c <- letter
  return $ Bvar c) <|> (do
  _ <- string "true"
  return $ Bvalue True) <|> (do
  _ <- string "false"
  return $ Bvalue False)

bcon :: Parser Bexp
bcon = do
  ex1 <- bdis
  exs <- many (do
    _ <- char '&'
    ex2 <- bdis
    return ex2)
  return $ toAnd (ex1:exs)

toAnd :: [Bexp] -> Bexp
toAnd [] = error ("No input")
toAnd [x] = x
toAnd (x:xs) = And x (toAnd xs)

bexp :: Parser Bexp
bexp = do
  ex1 <- bcon
  exs <- many (do
    _ <- char '|'
    ex2 <- bcon
    return ex2)
  return $ toOr (ex1:exs)

toOr :: [Bexp] -> Bexp
toOr [] = error ("No input")
toOr [x] = x
toOr (x:xs) = Or x (toOr xs)

anBexp :: String -> Maybe Bexp
anBexp str = case (parse fullBe "" str) of
                Left _ -> Nothing
                Right ex -> Just ex

-- Task 3 -----------------------------------------
type Name = String
type Attributes = [(String, String)]
data XML = Text String | Element Name Attributes [XML] deriving (Eq, Show)
fullXML :: Parser XML
fullXML = try (do
  spaces
  el <- element
  spaces
  eof
  return el)

text :: Parser String
text = many1 (noneOf "<>")

value :: Parser String
value = many1 (noneOf "\"")

fullValue :: Parser String
fullValue = do
  _ <- char '"'
  v <- value
  _ <- char '"'
  return v

name :: Parser Name
name = do
  x <- letter
  xs <- many (do
    c <- letter <|> digit <|> char '.' <|> char '-'
    return c)
  return $ x:xs

element :: Parser XML
element = do
  lookAhead (notFollowedBy (string "</"))
  _ <- char '<'
  nm <- name
  attr <- many attrib
  _ <- char '>'
  xmls <- many xml
  _ <- string ("</"++nm++">")
  return $ Element nm attr xmls

attrib :: Parser (String, String)
attrib = do
  spaces
  nm <- name
  spaces
  _ <- char '='
  spaces
  val <- fullValue
  return (nm, val)

xml :: Parser XML
xml = element <|> do
  txt <- text
  return $ Text txt

anXML :: String -> Maybe XML
anXML str = case (parse fullXML "" str) of
               Left _ -> Nothing
               Right res -> Just res

-- SPL Language -----------------------------------
data Value = I Int | B Bool deriving (Show, Eq)
data Exp = Var String     -- variable
         | Const Value    -- constant
         | Op Exp Bop Exp -- operation
                 deriving (Show, Eq)

-- Binary operators
data Bop =  Plus | Minus | Times | Div
          | Gt | Ge | Lt | Le| Eql | Ba | Bo
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

{- lexicon
  symbol  = ';' | '{' | '}' | '(' | ')'
  identif =  char {digit | char}
  keyword = "int" | "bool" | "if" | "while" | "for" | "else" | "true" | "false"
  iden    =  identif .... not "int" "bool" "if" "while" "for" "else" "true" "false"
  number  = digit { digit }.
  mulOp   = "*" | "/".
  addOp   = "+" | "-".
  relOp   = "<" | "<=" | ">" | ">=" | "=="
  disOp   = "&"
  conOp   = "|"
  typev   = "int" | "bool"
-}

iden :: Parser String
iden = try( do {nm <- identif;
                if (any(nm==) ["int","bool","if","while","for","else","True","False"])
                    then unexpected ("reserved word " ++ show nm)
                    else return nm
               } )

oper  :: String -> Bop -> Parser Bop
oper str bop = do {_ <- string str; return bop}

mulOp :: Parser Bop
mulOp = (oper "*" Times) <|> (oper "/" Div)

disOp :: Parser Bop
disOp = (oper "&" Ba)

conOp :: Parser Bop
conOp = (oper "|" Bo)

-- recognize all empty characters at the end
lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces; return a}

-- :type Op -----> Exp -> Bop -> Exp -> Exp
-- :type flip Op -----> Bop -> Exp -> Exp -> Exp
expOp :: Parser Bop -> Parser (Exp -> Exp -> Exp)
expOp p = do {x <- lexem p; return (flip Op x)}

symbol :: Char ->  Parser ()
symbol ch = lexem (char ch >> return ())

keyword :: String -> Parser ()
keyword st = try( lexem( string st >> notFollowedBy alphaNum))

typev :: Parser Type 
typev = do {keyword "int"; return It}
        <|> do {keyword "bool"; return Bt}

-- Task 4 -----------------------------------------
identif :: Parser String
identif = do
  x <- letter
  xs <- many (do
     c <- letter <|> digit
     return c)
  return $ x:xs

number :: Parser Int
number = do
  s <- many1 digit
  return $ read s

addOp :: Parser Bop
addOp = (oper "+" Plus) <|> (oper "-" Minus)

relOp :: Parser Bop  
relOp = (do {lookAhead (notFollowedBy $ string "<="); oper "<" Lt}) <|>
        (oper "<=" Le) <|>
        (do {lookAhead (notFollowedBy $ string ">="); oper ">" Gt}) <|>
        (oper ">=" Ge) <|>
        (oper "==" Eql)

{- expressions
  factor = '(' expr ')' | number | "true" | "false" | iden
  term   = factor { mulOp factor }
  relat  = term { addOp term }
  conj   = relat [relOp relat]
  disj   = conj { conOp conj}
  expr   = disj { disOp disj}
-}

factor :: Parser Exp
factor = do { symbol '('; x <- expr; symbol ')'; return x}
     <|> do {nm <- lexem number; return (Const (I nm))}
     <|> do {keyword "true"; return (Const (B True))}
     <|> do {keyword "false"; return (Const (B False))}
     <|> do {cs <- lexem iden; return (Var cs) }
     <?> "factor"

-- Task 5 -----------------------------------------
expHelper :: Parser Exp -> Parser Bop -> Exp -> Parser Exp
expHelper expParser opParser first = do
  option first (do
    op <- opParser
    spaces
    second <- expParser
    res <- expHelper expParser opParser (Op first op second)
    return res)

term :: Parser Exp
term = do
  f1 <- factor
  res <- expHelper factor mulOp f1
  return res

relat :: Parser Exp
relat = do
  t1 <- term
  res <- expHelper term addOp t1
  return res

conj :: Parser Exp
conj = do
  a1 <- relat
  res <- expHelper relat relOp a1
  return res

disj :: Parser Exp
disj = do
  c1 <- conj
  res <- expHelper conj conOp c1
  return res

expr :: Parser Exp
expr = do
  d1 <- disj
  res <- expHelper disj disOp d1
  return res

{- operators
  stmt   = "for" forSt | "while" whileSt | "if" ifSt
         | iden assSt | blockSt
  forSt  = '(' stmt ';' expr ';' stmt ')' stmt
  whileSt= '(' expr ')' stmt
  ifSt   = '(' expr ')' stmt "else" stmt
  assSt  = "++" | ":=" expr
  blockSt= '{' {defin} listSt '}'
  defin  = type iden ';'
  listSt = stmt {';' stmt}
  program= stmt eos
-}

stmt :: Parser Stmt
stmt = do {keyword "for"; forSt}
       <|> do {keyword "while"; whileSt}
       <|> do {keyword "if"; ifSt}
       <|> do {var <- lexem iden; assignSt var}
       <|> blockSt
       <?> "statement"

-- Task 6 -----------------------------------------
forSt :: Parser Stmt
forSt = do
  spaces
  symbol '('
  initSt <- stmt
  spaces
  symbol ';'
  check <- expr
  spaces
  symbol ';'
  stepSt <- stmt
  spaces
  symbol ')'
  bodySt <- stmt
  case initSt of
    Assign nm _ -> return $ Block [(nm,It)] [For initSt check stepSt bodySt]
    _ -> unexpected "Non-initializing statement in for loop."

whileSt :: Parser Stmt
whileSt = do
  spaces
  symbol '('
  ex <- expr
  spaces
  symbol ')'
  st <- stmt
  return $ While ex st

ifSt :: Parser Stmt
ifSt = do
  spaces
  symbol '('
  ex <- expr
  spaces
  symbol ')'
  st1 <- stmt
  spaces
  keyword "else"
  st2 <- stmt
  return $ If ex st1 st2

assignSt :: String -> Parser Stmt
assignSt s = (do
  spaces
  _ <- string "++"
  return $ Incr s) <|> (do
  spaces
  _ <- string ":="
  spaces
  ex <- expr
  return $ Assign s ex)

blockSt :: Parser Stmt
blockSt = do
  symbol '{'
  d <- many defin
  st <- listSt
  symbol '}'
  return $ Block d st

defin :: Parser (String, Type)
defin = do
  t <- typev
  nm <- iden
  symbol ';'
  return (nm,t)

listSt :: Parser [Stmt]
listSt = do
  spaces
  st1 <- stmt
  sts <- many (do
    symbol ';'
    st2 <- stmt
    return st2)
  return $ st1:sts

-- Main Functions ---------------------------------
program :: Parser Stmt
program = do {spaces; r <- stmt; eof; return r}

parseSPL :: String -> Either ParseError Program
parseSPL s = parse program "" s

--- Test Data -------------------------------------
casablanca :: String
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML
casablancaParsed
  = Element "film"
            [("title","Casablanca")]
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]

power :: String
power =
   "{ int b; int e; int out; b := 6; e := 5; out:= 1;\
   \  for (i:=0; i<e; i++) out := out*b   \
   \}"

powerAST :: Program 
powerAST = Block [("b",It),("e",It),("out",It)]
              [Assign "b" (Const (I 6)), Assign "e" (Const (I 5)), Assign "out" (Const(I 1)),
               Block [("i",It)]
                     [For (Assign "i" (Const(I 0))) (Op (Var "i") Lt (Var "e")) (Incr "i")
                           (Assign "out" (Op (Var "out") Times (Var "b")))
                     ]
              ]

squareRoot :: String
squareRoot =
   "{int a; int b; a := 317; b := 0;\
   \  {bool c; c:=true; while(c) {b++; c:= a >= b*b}};\
   \  b := b-1\
   \ }"

squareRootAST :: Program
squareRootAST = Block [("a",It),("b",It)]
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
