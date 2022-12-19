https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module PrinterImpl where

import AST
import ParserImpl

-- do not change the type!

optable1 = OpTable [(FNone, ["<=", "<"]),(FLeft, ["+", "-"]),(FLeft, ["*"]),(FRight, ["**"])] 
 
printTerm :: OpTable -> Term -> String
printTerm (OpTable o) term = hlpr False (map snd o) term 
--------- helper function 

hlpr :: Bool -> [[FName]] -> Term -> String
hlpr b opll t = case t of 
   TVar x      -> x
   TNum x      -> case  x > 0 of 
                    False -> '~' : show (abs x)
                    _     -> show x
   TFun f ts   -> case elem f opl of 
                    False -> f ++ "(" ++ tfunPrint_1 opll ts ++ ")"
                    _     -> case ts of
                              [t1,t2]  -> case b of 
                                            True -> "(" ++ hlpr (leftcompare  opll f t1) opll t1
                                                   ++ f ++ hlpr (rightcompare opll f t2) opll t2 ++ ")"
                                            _    ->        hlpr (leftcompare  opll f t1) opll t1
                                                   ++ f ++ hlpr (rightcompare opll f t2) opll t2
                              _        -> error "Have no 2 parms in ts."
 where opl = flatten opll
    

leftcompare :: [[FName]] -> FName {-- outter fname --} -> Term {-- inner Term --} -> Bool
leftcompare opll n1 t
       = case elem n1 opl of 
            True  -> case t of 
              TFun n2 _ -> case elem n2 opl of 
                     True -> let iin1 = inindex opll n1
                                 iin2 = inindex opll n2
                             in case iin1 == iin2 of 
                                 True -> False
                                 _    -> case iin1 > iin2 of 
                                          True -> True 
                                          _    -> False
                     _    -> False
              _         -> False
            _     -> False
    where opl = flatten opll               

rightcompare :: [[FName]] -> FName {-- outter fname --} -> Term {-- inner Term --} -> Bool
rightcompare opll n1 t
       = case elem n1 opl of 
            True  -> case t of 
              TFun n2 _ -> case elem n2 opl of 
                     True -> let 
                                 in1 = findindex opl n1
                                 in2 = findindex opl n2
                                 iin1 = inindex opll n1
                                 iin2 = inindex opll n2
                             in case iin1 == iin2 of 
                                 True -> case in1 == in2 of 
                                          True -> False 
                                          _    -> True
                                 _    -> case iin1 > iin2 of 
                                          True -> True 
                                          _    -> False
                     _    -> False
              _         -> False
            _     -> False
    where opl = flatten opll    
    
    
inindex ::(Eq a) => [[a]] -> a -> Int    
inindex l t = inindex' l t 0
    
inindex' :: (Eq a) => [[a]] -> a -> Int -> Int
inindex' []  _ _    = error "There is no this element."
inindex' (h:ts) t n
        | t `elem` h  = n
        | otherwise   = inindex' ts t (n+1)        
    
findindex :: (Eq a) => [a] -> a -> Int    
findindex l t = findindex' l t 0
    
findindex' :: (Eq a) => [a] -> a -> Int -> Int
findindex' []  _ _    = error "There is no this element."
findindex' (h:ts) t n
        | h == t     = n
        | otherwise  = findindex' ts t (n+1)        
            
tfunPrint_1 :: [[FName]] -> [Term] -> String
tfunPrint_1 opll ts@(h:tail) = case length ts > 1 of
                        True  -> hlpr False opll h ++ "," ++ tfunPrint_1 opll tail
                        _     -> hlpr False opll h

flatten :: [[a]] -> [a]
flatten []    = []
flatten (h:t) = h ++ flatten t
