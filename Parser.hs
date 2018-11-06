module Parser (parse) where -- only expose the top-level parsing function

import Combinators
import qualified Tokenizer as T
import Prelude hiding (lookup, (>>=), map, pred, return, elem)

data AST = ASum T.Operator AST AST
         | AProd T.Operator AST AST
         | AAssign Char AST
         | ANum Integer
         | AIdent Char
         | APow T.Operator AST AST
         | AUMinus T.Operator AST

-- TODO: Rewrite this without using Success and Error
parse :: String -> Maybe (Result AST)
parse input =
  case input of
    [] -> Nothing
    _ -> case expression input of
           Success (tree, ts') ->
             if null ts'
             then Just (Success tree)
             else Just (Error ("Syntax error on: " ++ show ts')) -- Only a prefix of the input is parsed
           Error err -> Just (Error err) -- Legitimate syntax error

expression :: Parser AST
expression =
  ( identifier >>= \(AIdent i) ->
    assignment |>
    expression >>= \e -> return (AAssign i e)
  )
  <|>   ( term       >>= \l  -> -- Here the identifier is parsed twice :(
        plusMinus  >>= \op ->
        expression >>= \r  -> return (ASum op l r)
      )
  <|> term

term :: Parser AST
term =
  -- make sure we don't reparse the factor (Term -> Factor (('/' | '*') Term | epsilon ))
  term2 >>= \l ->
  ( ( divMult >>= \op ->
      term2    >>= \r  -> return (AProd op l r)
    )
    <|> return l
  )
  
  
term2 :: Parser AST
term2 =
  factor >>= \l ->
  ( ( pow    >>= \op ->
      factor >>= \r -> return (APow op l r)
    )
    <|> return l
  )
  
  

factor :: Parser AST
factor =
  ( uminus >>= \op  ->
    factor >>= \fact -> return (AUMinus op fact)
  )
  <|> ( lparen |>
    expression >>= \e ->
    rparen |> return e -- No need to keep the parentheses
   )
  <|> identifier
  <|> digit

digit :: Parser AST
digit      = map (ANum   . T.digit) (sat T.isDigit elem)

identifier :: Parser AST
identifier = map (AIdent . T.alpha) (sat T.isAlpha elem)

lparen :: Parser Char
lparen = char '('

rparen :: Parser Char
rparen = char ')'

assignment :: Parser Char
assignment = char '='

plusMinus :: Parser T.Operator
plusMinus = map T.operator (char '+' <|> char '-')

divMult :: Parser T.Operator
divMult   = map T.operator (char '/' <|> char '*')

uminus :: Parser T.Operator
uminus = map T.operator (char '-')

pow :: Parser T.Operator
pow = map T.operator (char '^')




instance Show AST where
  show tree = "\n" ++ show' 0 tree
    where
      show' n t =
        (if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  AUMinus i x   -> showOp i : "\n" ++ show' (ident n) x
                  ASum  op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  APow  op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AProd op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AAssign  v e -> v : " =\n" ++ show' (ident n) e
                  ANum   i     -> show i
                  AIdent i     -> show i)
      ident = (+1)
      showOp T.Plus  = '+'
      showOp T.Minus = '-'
      showOp T.Mult  = '*'
      showOp T.Div   = '/'
      showOp T.Pow   = '^'
