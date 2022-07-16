{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant ==" #-}

module Confimp where

import System.Environment
import System.Exit ()
import Data.Maybe

import Lib

data Conf = Conf {
    rule :: Maybe Int,
    start :: Maybe Int,
    line :: Maybe Int,
    window :: Maybe Int,
    move :: Maybe Int,
    error_test :: Bool
} deriving (Show)

defaultConf :: Conf
defaultConf = Conf {rule = Just 0, start = Just 0, line = Just
    (-1), window = Just 80, move = Just 0, error_test = False}

getOpts :: Conf -> [ String ] -> Maybe Conf
getOpts conf ("--start":x:xs) = getOpts conf{start = readInt x} xs
getOpts conf ("--rule":x:xs) = getOpts conf{rule = readInt x} xs
getOpts conf ("--lines":x:xs) = getOpts conf{line = readInt x} xs
getOpts conf ("--window":x:xs) = getOpts conf{window = readInt x} xs
getOpts conf ("--move":x:xs) = getOpts conf{move = readInt x} xs
getOpts conf [] = Just conf
getOpts x xc = Just (x{error_test = True})