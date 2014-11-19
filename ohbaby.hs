{-
  Copyright 2014, Esteban I Ruiz Moreno, exio4.com@gmail.com
  Licensed under the Eiffel Forum License 2.
-}

module Main (
  main,
  eval,
  parse
) where

import System.IO.Unsafe -- for lazy readlines
import System.Console.Readline

import Data.Word
import Data.Bits

version :: String
version = "0.0.1"

data OB = Oh | Baby | OhBaby | Data Word8 | Newline
  deriving (Eq,Show)

showOB' :: OB -> String
showOB' Oh          = "oh"
showOB' Baby        = "baby"
showOB' OhBaby      = "ohbaby"
showOB' (Data n)    = show n
showOB' Newline     = "newline"
  
  -- naive shitty parser(tm)
parse :: String -> [OB]
parse [] = []
parse ('\n':xs) = Newline:parse xs
parse ('o':'h':' ':'b':'a':'b':'y':xs) = OhBaby:parse xs
parse ('o':'h':xs) = Oh:parse xs
parse ('b':'a':'b':'y':xs) = Baby:parse xs
parse xs@(_:xs')
    | null n    = parse xs'
    | otherwise = Data (read n) : parse ys   
  where (n,ys) = span (`elem` ['0'..'9']) xs

eval :: [OB] -> IO [Word8]
eval = eval' []
  where
    eval' :: [Word8] -> [OB] -> IO [Word8]
    eval' qs       [] = return qs 
    eval' qs       ((Data n):xs) =
      eval' (n:qs)         xs
    eval' (x:y:qs) (Oh      :xs) =
      eval' ((x .&. y):qs) xs
    eval' (x:y:qs) (Baby    :xs) =
      eval' ((x .|. y):qs) xs
    eval' (x:qs)   (OhBaby  :xs) =
      eval' ((complement x):qs) xs
    eval' qs       (Newline :xs) =
      print qs >>
      eval' qs xs
    eval' qs  (x:cmd) =
      putStrLn "something fucked up with the command:" >>
        putStrLn (showOB' x) >>
        putStr "stack:" >> print qs >>
        putStrLn "[SKIPPING]" >>
        eval' qs cmd

readlines :: String -> (String -> IO (Either (String -> String) String)) -> IO String
readlines prompt callback =
       readline prompt >>= \x ->
      case x of
           Nothing ->
               return []
           Just (':':xs) ->
               callback xs >>= \x' -> case x' of
                  Left  f -> next False f (':':xs)
                  Right n -> return n
           Just str -> next True id str
  where
    next sk f str =
         addHistory str >>
         unsafeInterleaveIO (readlines (f prompt) callback) >>= \x ->
         return
          (if sk
            then (str ++ '\n':x)
            else x)
      
repl :: IO ()
repl = fmap parse (readlines ">> " cb) >>= eval >>= putStrLn.("Remaining stack: \n\t" ++).show
  where cb :: String -> IO (Either (String -> String) String)
        cb ('q':_) =
          putStrLn "Leaving ..." >> return (Right "")
        cb ('c':' ':xs) =
          return $ Left (const ((filter (/= ' ') xs) ++ " "))
        cb ('h':_) =
          putStrLn "Help ::" >>
          putStrLn "\t:q       -> Quit REPL"     >>
          putStrLn "\t:c <str> -> Change prompt" >>
          putStrLn "\t:h       -> This help"     >>
          return (Left id)
        cb _   =
          return $ Left id
          
          
main :: IO ()
main = putStrLn ("oh baby v" ++ version) >> repl