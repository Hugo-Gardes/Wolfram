{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant ==" #-}
module Main where

import System.Environment
import System.Exit
import Data.Maybe

import Lib
import Confimp
import Rules
import Rule33
import Rule90
import Rule110

errorhandling :: [String] -> IO()
errorhandling xs | lentab xs == 0 = exitWith (ExitFailure 84)
                 | odd (lentab xs) = exitWith (ExitFailure 84)
                 | not (tabcontain xs "--rule") = exitWith (ExitFailure 84)
                 | otherwise = putStr ""

ckru :: Maybe Int -> Bool
ckru (Just 30) = True
ckru (Just 90) = True
ckru (Just 110) = True
ckru x = False

testerror :: Maybe Conf -> IO()
testerror conf
    | error_test (fromMaybe defaultConf conf) = exitWith (ExitFailure 84)
    | not$ckru (rule (fromMaybe defaultConf conf)) = exitWith (ExitFailure 84)
    | otherwise = putStr ""

launch :: Maybe Conf -> Int -> IO ()
launch conf 30 = rule33exec conf (fromMaybe 0 (line (fromMaybe defaultConf
    conf))) "*" (fromMaybe 0 (start (fromMaybe defaultConf conf)))
launch conf 90 = rule90exec conf (fromMaybe 0 (line (fromMaybe defaultConf
    conf))) "*" (fromMaybe 0 (start (fromMaybe defaultConf conf)))
launch conf 110 = rule110exec conf (fromMaybe 0 (line (fromMaybe defaultConf
    conf))) "*" (fromMaybe 0 (start (fromMaybe defaultConf conf)))
launch conf x = putStrLn "not implement" >> exitWith (ExitFailure 84)

main :: IO()
main = do
    args <- getArgs;
    errorhandling args
    let se = getOpts defaultConf args
    testerror se
    launch se (fromMaybe 0 (rule (fromMaybe defaultConf se)))
