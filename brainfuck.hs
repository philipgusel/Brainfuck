module Main where

import Data.Char (ord)
import System.Environment (getArgs)

ft (a,_,_) = a
st (_,a,_) = a
tt (_,_,a) = a

tapelength = 1000

toascii i 
        | i < 1000 = toEnum (fromIntegral i)
        | otherwise = '#'


replace pos newVal list = take (fromIntegral pos) list ++ newVal : drop (fromIntegral (pos+1)) list

main = do
        s <- getArgs
        parseargs s

parseargs [] = do putStrLn "no file provided"
parseargs (f:fs) = do
        f <- readFile f
        run f

filerun f = parseargs [f]

run s = do doprog (s,0,0,(replicate tapelength 0),(False,0,'p'))

doprog (p,i,tp,t,io)
        | i < (length p) = do 
                                let n = newstate (p,i,tp,t,io)
                                d <- doio n
                                doprog d
        | otherwise = do 
                        return ()

doio (p,i,tp,t,io)
        | ft io == True && tt io == 'p' = do 
                                                putChar (toascii (abs (st io)))
                                                return (p,i,tp,t,(False,0,'p'))
        | ft io == True && tt io == 'g' = do 
                                                s <- getChar
                                                return (p,i,tp,t,(True,(ord s),'r'))
        | otherwise = do return (p,i,tp,t,io)

newstate (p,i,tp,t,io)
        | (tt io) == 'r' = newstate (p,i,tp,(replace tp (st io) t),(False,0,'p'))
        | (p !! i) == '>' = (p,(i+1),(abs(mod (tp+1) tapelength)),t,io)
        | (p !! i) == '<' = (p,(i+1),(abs(mod (tp-1) tapelength)),t,io)
        | (p !! i) == '+' = (p,(i+1),tp,(replace tp ((t !! tp)+1) t),io)
        | (p !! i) == '-' = (p,(i+1),tp,(replace tp ((t !! tp)-1) t),io)
        | (p !! i) == '.' = (p,(i+1),tp,t,(True,(t !! tp),'p'))
        | (p !! i) == ',' = (p,(i+1),tp,t,(True,0,'g'))
        | (p !! i) == '[' && (t !! tp) /= 0 = (p,(i+1),tp,t,io)
        | (p !! i) == ']' && (t !! tp) == 0 = (p,(i+1),tp,t,io)
        | (p !! i) == '[' && (t !! tp) == 0 = (p, (findnextjmpclose p i),tp,t,io)
        | (p !! i) == ']' && (t !! tp) /= 0 = (p, (findprevjmpopen p (i-1) 0), tp, t, io)
        | otherwise = (p,(i+1),tp,t,io)

findprevjmpopen p i n
        | i == 0 = 0
        | (p !! i) == ']' = findprevjmpopen p (i-1) (n+1)
        | (p !! i) == '[' && n == 0 = i
        | (p !! i) == '['  = findprevjmpopen p (i-1) (n-1)
        | otherwise = findprevjmpopen p (i-1) n

findnextjmpclose p i
        | i == ((length p)-1) = ((length p)-1)
        | (p !! i) == ']' = i
        | otherwise = findnextjmpclose p (i+1)

s = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.+++."
