{-# LANGUAGE DataKinds #-}
module Rule33 where

import Data.Maybe

import Confimp
import Rules
import Displayline
import Lib

makeline :: [Char] -> [Char] -> [Char] -> [Char]
makeline line fline cline
    | len cline /= 3 = fline
    | otherwise = makeline (tail line) (rule30 cline:fline)
    (take 3 (tail line))

indexcalc :: Int -> Int -> Int -> Int
indexcalc win seg move
    | odd seg && even win = (win `div` 2 - seg `div` 2 + 1) + move
    | otherwise = (win `div` 2 - seg `div` 2) + move

rule33exec :: Maybe Conf -> Int -> [Char] -> Int -> IO()
rule33exec conf 0 lastline start = return ()
rule33exec conf nbrline lastline 0 = displayline lastline w m (indexcalc w (len lastline) m) >>
    rule33exec conf (nbrline - 1) (reverse (makeline (' ':' ':lastline ++ "  ") [] (take 3 (' ':' ':lastline ++ "  ")))) 0
    where
        m = fromMaybe 0 (move (fromMaybe defaultConf conf))
        w = fromMaybe 0 (window (fromMaybe defaultConf conf))
rule33exec conf nbrline lastline start = rule33exec conf nbrline
    (reverse (makeline (' ':' ':lastline ++ "  ") [] (take 3 (' ':' ':lastline ++ "  ")))) (start - 1)
