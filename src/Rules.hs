module Rules where

rule30 :: [Char] -> Char
rule30 ['*', '*', '*'] = ' '
rule30 ['*', '*', ' '] = ' '
rule30 ['*', ' ', ' '] = '*'
rule30 [' ', '*', '*'] = '*'
rule30 [' ', '*', ' '] = '*'
rule30 [' ', ' ', '*'] = '*'
rule30 [' ', ' ', ' '] = ' '
rule30 ['*', ' ', '*'] = ' '
rule30 x = ' '

rule90 :: [Char] -> Char
rule90 ['*', '*', '*'] = ' '
rule90 ['*', '*', ' '] = '*'
rule90 ['*', ' ', ' '] = '*'
rule90 [' ', '*', '*'] = '*'
rule90 [' ', '*', ' '] = ' '
rule90 [' ', ' ', '*'] = '*'
rule90 [' ', ' ', ' '] = ' '
rule90 ['*', ' ', '*'] = ' '
rule90 x = 'e'

rule110 :: [Char] -> Char
rule110 ['*', '*', '*'] = ' '
rule110 ['*', '*', ' '] = '*'
rule110 ['*', ' ', ' '] = ' '
rule110 [' ', '*', '*'] = '*'
rule110 [' ', '*', ' '] = '*'
rule110 [' ', ' ', '*'] = '*'
rule110 [' ', ' ', ' '] = ' '
rule110 ['*', ' ', '*'] = '*'
rule110 x = 'e'