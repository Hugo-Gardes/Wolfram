{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant ==" #-}

module Displayline where

import System.Environment
import System.Exit
import Data.Maybe

import Lib

mycatloop :: Int -> [Char] -> [Char] -> [Char] -> Int -> Int -> Int -> [Char]
mycatloop siz init end lact actind stind endind
    | siz <= actind = end
    | (actind >= stind) && (actind < endind) && not (null lact) =
        mycatloop siz (tail init) (head lact:end)
    (tail lact) (actind + 1) stind endind
    | not (null init) = mycatloop siz (tail init) (head init:end)
    lact (actind + 1) stind endind
    | otherwise = end

mycat :: Int -> [Char] -> [Char] -> [Char] -> Int -> Int -> Int -> [Char]
mycat siz start final line actind stind endind
    | actind > siz = final
    | len line > 0 && actind < endind = mycatloop siz
    (tail start) (head line : final) (tail line) (actind + 1) stind endind
    | len start > 0 = mycatloop siz (tail start)
    (head line : final) (tail line) (actind + 1) stind endind
    | otherwise = final

initl :: Int -> [Char] -> [Char]
initl 0 final = final
initl nbr final = ' ':initl (nbr - 1) final

conv :: [Char] -> Int -> [Char]
conv [] nbr = []
conv start nbr
    | nbr + 1 >= 1 = start
    | otherwise = conv (tail start) (nbr + 1)

displayline :: [Char] -> Int -> Int -> Int-> IO ()
displayline str nbrcells mv index
    | index - 1 < 1 = putStrLn (reverse (mycat nbrcells
    (initl nbrcells []) [] (conv str (index - 1)) 0 (index - 1)
    (index - 1 + len str)))
    | index - 1 < nbrcells = putStrLn (reverse (mycatloop nbrcells
    (initl nbrcells []) [] str 0 (index - 1) ((index - 1) + len str)))
    | otherwise = putStrLn (initl nbrcells [])