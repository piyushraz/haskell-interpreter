module HaskellInterp
  (
    run,
    eval
  )
where

import HaskellInterpTypes(Expr(..), Value(..), Env)
import qualified Data.Map (lookup, insert, empty)


-- | Runs a Godel expression by calling `eval` with the empty environment
run :: Expr -> Value
run e = eval Data.Map.empty e



-- ----------------------------| CODING BELOW | -------------------------------

eval :: Env -> Expr -> Value

eval env (Literal v) = v 

eval env (Plus a b) = case (eval env a, eval env b) of
  (Error x, _) -> Error x
  (_, Error y) -> Error y
  (Num x, Num y) -> Num (x + y)
  _ -> Error "Plus" -- (Plus a b) has either a or b not evaluating to a number

eval env (Times a b) = case (eval env a, eval env b) of
  (Error x, _) -> Error x
  (_, Error y) -> Error y
  (Num x, Num y) -> Num (x * y)
  _ -> Error "Times" -- (Times a b) has either a or b not evaluating to a number

eval env (Equal a b) = case (eval env a, eval env b) of
  (Error x, _) -> Error x
  (_, Error y) -> Error y
  (x, y) -> if x == y then T else F

eval env (Cons a b) = case (eval env a, eval env b) of
  (Error x, _) -> Error x
  (_, Error y) -> Error y
  (x, y) -> Pair x y

eval env (First expr) = case eval env expr of
  (Pair x _) -> x
  (Error e) -> Error e
  _ -> Error "First" -- (First e) has e not evaluating to a pair

eval env (Rest expr) = case eval env expr of
  (Pair _ y) -> y
  (Error e) -> Error e
  _ -> Error "Rest" -- (Rest e) has e not evaluating to a pair

eval env (If cond expr alt) = case eval env cond of
  T -> eval env expr
  F -> eval env alt
  (Error e) -> Error e

eval env (Var name)  = case (Data.Map.lookup name env) of
    Just a  -> a 
    Nothing -> Error "Var" -- (Var name) does not exist in the environment

eval env (Lambda params body) 
    | params == unique params = Closure params env body
    | otherwise = Error "Lambda" -- Function expression contains two or more parameters with the same name


eval env (App fnExpr argExprs) = case (eval env fnExpr) of
  Closure params cenv body -> helperApp env cenv params body argExprs
  Error e -> Error e
  _ -> Error "App" -- (Apply fnExpr argExprs) has fnExpr not evaluating to a closure


-- ----------------------------| CODING ABOVE | -------------------------------


-- | Helper function to obtain a list of unique elements in a list
-- Example:
--   ghci> unique [1, 2, 3, 4]
--   [1,2,3,4]
--   ghci> unique [1, 2, 3, 4, 4]
--   [1,2,3,4]
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
  | elem x xs = unique xs
  | otherwise = x : unique xs

helperApp :: Env -> Env -> [String] -> Expr -> [Expr] -> Value
helperApp env cenv params body argExprs 
  | length params == length argExprs =
        let output = map (eval env) argExprs
            fenv = foldr (\(key, value) acc -> Data.Map.insert key value acc) cenv (zip params output)
        in eval fenv body
  | otherwise = Error "App" -- (Apply fnExpr argExprs) has fnExpr not evaluating to a closure