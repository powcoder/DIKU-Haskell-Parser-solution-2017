https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module SemanticsImpl where

import AST
import Control.Monad

import Data.Either
import Data.List

-- Put your implementation of the rewrite engine here. Feel free to
-- define your own auxilary functions as needed, but do not modify
-- the type definitions and declarations of the required individual
-- functions from what's specified here and in the assignment text.

---- Global monad and related functions

type GEnv = [Rule]

newtype Global a = Global {runGlobal :: GEnv -> Either (Maybe ErrMsg) a}

instance Monad Global where
  return v = Global (\_ -> Right v)
  a >>= f = Global (\g -> case runGlobal a g of 
                             Left x  -> Left x
                             Right v -> runGlobal (f v) g) 
                           
-- You may modify these if you want, but it shouldn't be necessary
instance Functor Global where fmap = liftM
instance Applicative Global where pure = return; (<*>) = ap
  
getRules :: Global [Rule]
getRules = Global (\v -> Right v)

failS :: Global a
failS = Global (\_ -> Left Nothing)

failH :: ErrMsg -> Global a
failH err = Global (\_ -> Left (Just err))

tryS :: Global a -> Global a -> Global a
tryS m1 m2 = do
              rule <- getRules
              case isRight (runGlobal m1 rule) of 
                 True  -> m1
                 False -> case (runGlobal m1 rule) of 
                            Left Nothing     -> m2
                            Left  (Just err) -> failH err                    
      
---- Local monad and related functions

type LEnv = [(VName, Term)]

newtype Local a = Local {runLocal :: LEnv -> Global (a, LEnv)}

instance Monad Local where
  return v = Local (\lv -> Global (\_ -> Right (v,lv)))
  t >>= f  = Local (\lv -> Global (\gv -> 
        let a1 = runGlobal (runLocal t lv)  gv
        in case a1 of 
            Left (Just e) -> Left (Just e)
            Left Nothing     -> Left Nothing
            Right (v,l) ->runGlobal (runLocal (f v) l) gv               
                  )
     )

    
instance Functor Local where fmap = liftM
instance Applicative Local where pure = return; (<*>) = ap

inc :: Global a -> Local a
inc g = Local (\lg -> do 
                      a <- g
                      return (a,lg)
               )

contnc :: Local a -> Global a
contnc l = do
          (v,_) <- runLocal l []
          return v          
    
askVar :: VName -> Local Term
askVar v = Local (\lv -> case (findIndex (==v) (map fst lv)) of 
                              Just x -> return (snd (lv!!x),lv)
                              _      -> failH "Error happend in askVar function."
               )
    
tellVar :: VName -> Term -> Local ()
tellVar v t =  Local (\lg -> case (runGlobal (runLocal (askVar v) lg) []) of 
                          Left _        -> return ((),lg++[(v,t)])
                          Right  (x,_)  -> case x == t of 
                                             True  -> return ((),lg)
                                             False -> failS
               ) 
    
---- Matching and instantiation

matchTerm :: Term -> Term -> Local ()
matchTerm p t = case p of
    TNum _      -> return ()
    TVar v      -> tellVar v t
    TFun f1 ts1 -> case t of 
           TFun f2 ts2 -> case f1 == f2 of 
                      True -> zipWithM_ matchTerm ts1 ts2 
                      _    -> inc failS
           _           -> inc failS
             
    
instTerm :: Term -> Local Term
instTerm t = case t of
    TNum v    -> return (TNum v)
    TVar v    -> do {a<- askVar v;return a}
    TFun f ts -> do {a<- mapM instTerm ts;return (TFun f a)}

---- Conditions and rule aplication

evalCond :: PName -> [Term] -> Global [Term]
evalCond pn ts = 
          case ts of 
            [] -> failS
            _  -> case pn of
               "num" -> case arg1 of 
                    TNum _ -> return []
                    _      -> failH "Error happend in evalCond function : num."   
               "var" -> case arg1 of 
                    TVar _ -> return []
                    _      -> failH "Error happend in evalCond function : var."  
               "add" -> case (arg1,arg2) of 
                    (TNum n1,TNum n2) -> return [TNum (n1+n2)]
                    _                 -> failH "Error happend in evalCond function : add."  
               "mul" -> case (arg1,arg2) of 
                    (TNum n1,TNum n2) -> return [TNum (n1*n2)]
                    _                 -> failH "Error happend in evalCond function : mul." 
               _    -> failH "Error happend in evalCond function : unknown predicate." 
        where 
            arg1 = ts!!0
            arg2 = ts!!1        
    
applyRule :: Rule -> Term -> Global Term
applyRule (Rule t1 t2 []) t = contnc (do {matchTerm t1 t ;instTerm t2})
                              
---- Single-step term rewriting
rewriteTerm :: Term -> Global Term
rewriteTerm = undefined

---- Top-level interaction
processCmd :: Cmd -> GEnv -> (Rsp, GEnv)
processCmd (CRule r) ge = (Rsp [] Nothing,ge++[r])

                          
