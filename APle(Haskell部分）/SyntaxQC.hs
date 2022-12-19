https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
import AST
import Syntax

import Test.QuickCheck
import Control.Monad

-- Your code here
opt = OpTable [(FNone, ["<=", "<"]),(FLeft, ["+", "-"]),(FLeft, ["*"]),(FRight, ["**"])]

oplist :: [String]
oplist = ["<=", "<","+", "-","*","**"]

lchar :: Gen Char
lchar = frequency [ (26, choose ('a', 'z')) ]

lcharAnddigit :: Gen Char
lcharAnddigit = frequency ([ (26, choose ('a', 'z')) ] ++ [ (10, choose ('0', '9')) ])

vGen :: Gen String
vGen  = liftM2 (:) lchar (sized (\n -> replicateM n lcharAnddigit))

tfGen :: Gen String
tfGen = oneof ([liftM2 (:) lchar (sized (\n -> replicateM n lcharAnddigit))] ++ map pure oplist)
    
instance Arbitrary Term where
  arbitrary = oneof [ liftM TVar vGen, liftM TNum arbitrary, liftM2 TFun tfGen (replicateM 2 arbitrary) ]

checkTermParser :: Term -> Bool  
checkTermParser t = let c = parseStringTerm opt (printTerm opt t) in 
                      case c of
                        Left  _ -> False
                        Right a -> t == a
  
-- main test function, to make tests runnable as "runhaskell SyntaxQC"
main :: IO ()
main = quickCheck checkTermParser



