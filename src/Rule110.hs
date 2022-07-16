{-# LANGUAGE DataKinds #-}
module Rule110 where

import Data.Maybe

import Confimp
import Rules
import Displayline
import Lib

makeline :: [Char] -> [Char] -> [Char] -> [Char]
makeline line fline cline
    | len cline /= 3 = fline
    | otherwise = makeline (tail line) (rule110 cline:fline)
    (take 3 (tail line))

indexcalc :: Int -> Int -> Int -> Int
indexcalc win seg move
    | odd seg && even win = (win `div` 2 - seg `div` 2 + 1) + move
    | otherwise = (win `div` 2 - seg `div` 2) + move

rule110exec :: Maybe Conf -> Int -> [Char] -> Int -> IO()
rule110exec conf 0 lastline start = return ()
rule110exec conf nbrline lastline 0 = displayline lastline (fromMaybe 0 (window
    (fromMaybe defaultConf conf))) (fromMaybe 0 (move (fromMaybe defaultConf
    conf))) (indexcalc (fromMaybe 0 (window (fromMaybe defaultConf conf)))
    (len lastline) (fromMaybe 0 (move (fromMaybe defaultConf conf)))) >>
    rule110exec conf (nbrline - 1) (reverse (makeline
    (' ':' ':lastline ++ "  ")
    [] (take 3 (' ':' ':lastline ++ "  ")))) 0
rule110exec conf nbrline lastline start = rule110exec conf nbrline
    (reverse (makeline (' ':' ':lastline ++ "  ") [] (take 3
    (' ':' ':lastline ++ "  ")))) (start - 1)
