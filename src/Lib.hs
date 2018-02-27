{-
  Case study: compositionality and efficiency
  Define a function to convert an expression to a string
-}
module Lib where

-- hide some prelude names from the Show type class
import           Prelude hiding (show, shows)
import qualified Prelude (show)

-- | a datatype for simple arithmetic expressions
data Expr
  = Num Int
  | Var Char
  | Add Expr Expr
  | Mul Expr Expr

-- | naive conversion to string
-- compositional but inefficient
-- /O(n^2)/ for an expression of size /n/

show :: Expr -> String
show (Num n)
  = Prelude.show n
show (Var x)
  = [x]  
show (Add e1 e2)
  = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
show (Mul e1 e2)
  = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"

-- | eficient solution; worker function
-- returning a "difference list"  String -> String 
-- specification:
--   shows e xs = show e ++ xs
-- /O(n)/ for an expression of size /n/
--
shows :: Expr -> String -> String
shows (Num n) 
  -- \xs -> Prelude.show n ++ xs
  = (Prelude.show n ++)
shows (Var x) 
  --- \xs -> x : xs
  = (x:)
shows (Add e1 e2) 
    -- \xs -> "(" ++ showsExpr e1 ("+" ++ showsExpr e2 (")" ++ xs))
    =  ('(':) . shows e1 . ('+':) . shows e2 .  (')':) 
shows (Mul e1 e2) 
    -- \xs -> "(" ++ showsExpr e1 ("*" ++ showsExpr e2 (")" ++ xs))
    = ('(':) . shows e1 . ('*':) . shows e2 . (')':)

-- | efficient solution, toplevel wrapper
show' :: Expr -> String
show' e = shows e ""


-- some test cases

-- examples:
-- 1 + 2
example1 = Add (Num 1) (Num 2)

-- (1 + 2)*3
example2 = Mul (Add (Num 1) (Num 2)) (Num 3)

-- 2*(x+1)
example3 = Mul (Num 2) (Add (Var 'x') (Num 1))

-- | construct an expression of given size
sizedExpr :: Int -> Expr
sizedExpr 0  = Num 0
sizedExpr n  = Add (sizedExpr (n-1)) (Num n)

{-
-- Example GHCi session
-- displays allocations and time for each top-level reduction 
> :set +s
--
-- the naive solution appears to grows quadratically
--
> length $ show $ sizedExpr 1000
5894
(0.14 secs, 164,856,768 bytes)
> length $ show $ sizedExpr 2000
12894
(0.38 secs, 887,796,776 bytes)
> length $ show $ sizedExpr 4000
26894
(1.56 secs, 4,207,804,352 bytes)
--
-- the efficient solution appears to grow linearly
--
> length $ show' $ sizedExpr 1000
5894
(0.02 secs, 1,658,560 bytes)
> length $ show' $ sizedExpr 2000
12894
(0.03 secs, 3,144,656 bytes)
> length $ show' $ sizedExpr 4000
26894
(0.05 secs, 6,120,656 bytes)
-}




