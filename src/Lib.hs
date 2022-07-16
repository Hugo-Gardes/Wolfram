{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant ==" #-}

module Lib (
    len,
    readInt,
    myElem,
    myputstr,
    lentab,
    compstring,
    tabcontain
    ) where

import System.Environment
import System.Exit
import Data.Maybe

myElem :: Eq a => a -> [ a ] -> Bool
myElem a [] = False
myElem a (b:xs) = (a == b) || myElem a xs

readInt :: [ Char ] -> Maybe Int
readInt [] = Nothing
readInt (a:xs) = if isNumber (a:xs) == False
    then Nothing
    else Just (read (a:xs) :: Int)
    where
        isNumber :: [Char] -> Bool
        isNumber [] = True
        isNumber (a:xs) = if not (myElem a "0123456789-")
            then False
            else isNumber xs

myputstr :: [Char] -> IO()
myputstr [] = return ()
myputstr xs = putStrLn xs

lentab :: [String] -> Int
lentab [] = 0
lentab (x:xs) = 1 + lentab xs

len :: [Char] -> Int
len [] = 0
len (x:xs) = 1 + len xs

compstring :: String -> String -> Bool
compstring [] xs = True
compstring xc [] = True
compstring (x:xs) (y:ys) | len (x:xs) /= len (y:ys) = False
                         | x == y = compstring xs ys
                         | otherwise = False

tabcontain :: [String] -> String-> Bool
tabcontain [] xs = False
tabcontain (x:xs) xc | not (compstring x xc) = tabcontain xs xc
                     | otherwise = True