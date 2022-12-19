https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module ParserImpl where

import AST
import Text.ParserCombinators.ReadP as R
import Data.Char
import Control.Monad

parseStringTerm :: OpTable -> String -> Either ErrMsg Term
parseStringTerm opt str = parse (parseTerm opt) str

parseStringCmds :: OpTable -> String -> Either ErrMsg [Cmd]
parseStringCmds opt str = parse (parseCmds opt) str 
--------------------------------------------------------------------------------
optable = OpTable [(FNone, ["<=", "<"]),(FLeft, ["+", "-"]),(FLeft, ["*"]),(FRight, ["**"])]              

runParser :: ReadP a -> ReadS a
runParser = readP_to_S

parse :: ReadP a -> String -> Either String a
parse p str = case opt of
                 []    -> Left "ReadP error."
                 (x:_) -> Right (fst x)
            where opt = filter (\x-> snd x == "") (runParser p str) 
    
token :: ReadP a -> ReadP a
token p = do 
            skipSpaces
            a <- p
            skipSpaces
            return a

strToken :: String -> ReadP ()
strToken s = do skipSpaces; string s; skipSpaces
            
digits :: ReadP String
digits = munch1 isDigit

sign :: ReadP Integer
sign = (do {strToken "~"; return (-1)}) +++ return 1

parens :: ReadP p -> ReadP p
parens p = between (strToken "(") (strToken ")") p

parseName :: ReadP String
parseName = do 
                f <- satisfy isLetter -- first must be a letter
                r <- munch (\c -> isAlphaNum c ) -- rest
                return $ (f:r)
                
parseInt :: ReadP Integer
parseInt = do
            s <- sign
            d <- digits
            return $ (s * (read d))
--------------------------------------------------------------------------------
-- parseTerm :: OpTable -> ReadP Term
-- parseTerm opt =  parseFactor opt +++ arithmetic opt


-- arithmetic :: OpTable -> ReadP Term
-- arithmetic opt = prodOpChain (parseFactor opt) (parseOps opt)

-- prodOpChain :: ReadP Term -> [(Fixity,ReadP (Term -> Term -> Term))] -> ReadP Term
-- prodOpChain p []       = p
-- prodOpChain p (h:tail) = case fst h of 
       -- FRight -> prodOpChain (chainr1 p (snd h)) tail
       -- _      -> prodOpChain (chainl1 p (snd h)) tail               
                          
-- parseOps :: OpTable -> [(Fixity,ReadP (Term -> Term -> Term))]
-- parseOps (OpTable l)= [(f,choice $ map (\x -> do {strToken x;return (opc x)}) os)|(f , os) <- reverse l]

-- parseFactor :: OpTable -> ReadP Term   
-- parseFactor opt = parseTNum +++ parseTVar +++ parseTFun opt +++ parens (parseTerm opt)

-- opc :: String -> Term -> Term -> Term
-- opc op term1 term2 = TFun op [term1,term2]             
             
parseTNum :: ReadP Term             
parseTNum = do
             i <- parseInt
             return $ TNum i

parseTVar :: ReadP Term                 
parseTVar = do
             n <- parseName
             return $ TVar n   

parseTFun :: OpTable -> ReadP Term                 
parseTFun opt = do
             n <- parseName
             ts <-parens (parseTermz opt)
             return $ TFun n ts
             
parseTermz :: OpTable -> ReadP [Term]
parseTermz opt = sepBy (parseTerm opt) (strToken ",") 

parseTerms :: OpTable -> ReadP [Term]
parseTerms opt = sepBy1 (parseTerm opt) (strToken ",")
  
--------------------------------------------------------------------------------

parseCmds :: OpTable -> ReadP [Cmd]
parseCmds opt = (endBy (parseCmd opt) (strToken "\n")) +++ (sepBy (parseCmd opt) (strToken "\n"))
    
parseCmd :: OpTable -> ReadP Cmd
parseCmd opt = parseCRule opt +++ parseCQuery opt           

parseCRule :: OpTable -> ReadP Cmd
parseCRule opt = do
              r <- parseRule opt
              return $ CRule r
    
parseCQuery :: OpTable -> ReadP Cmd
parseCQuery opt = do
           t <- parseTerm opt
           b <- parseBool
           return $ CQuery t b

parseBool :: ReadP Bool      
parseBool = do { strToken "?"; return True } +++ do { strToken "??"; return False }   
          
parseRule :: OpTable -> ReadP Rule    
parseRule opt =  (parseRule_2 opt) +++ (parseRule_1 opt)
          
parseRule_1 :: OpTable -> ReadP Rule
parseRule_1 opt = do 
         term1 <- parseTerm opt 
         strToken "="
         term2 <- parseTerm opt 
         strToken "."
         return (Rule term1 term2 [])

parseRule_2 :: OpTable -> ReadP Rule
parseRule_2 opt = do 
     term1 <- parseTerm opt
     strToken "="
     term2 <- parseTerm opt
     strToken "|"
     parseConds <- parseConds opt
     strToken "."
     return (Rule term1 term2 parseConds)
    
parseConds :: OpTable -> ReadP [Cond]
parseConds opt = sepBy1 (parseCond opt) (strToken ",")

parseCond :: OpTable -> ReadP Cond
parseCond opt = parseCond_1 opt +++ parseCond_2 opt

parseCond_1 ::  OpTable -> ReadP Cond
parseCond_1 opt = do
                 pname <- parseName
                 pterm <- parens (parseTermz opt)
                 return (Cond pname pterm [])

parseCond_2 :: OpTable -> ReadP Cond
parseCond_2  opt = do
                 pname  <- parseName
                 strToken "("
                 pterm1 <- parseTermz opt 
                 strToken ";"
                 pterm2 <- parseTerms opt 
                 strToken ")"
                 return (Cond pname pterm1 pterm2)            