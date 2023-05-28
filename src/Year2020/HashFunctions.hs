module Year2020.HashFunctions where

import Data.Bits

import Year2020.Types

hash :: HashFun
hash x
  = op x'' 
  where
    op x = xor (shiftR x 16) x
    x'  = op x * 73244475
    x'' = op x' * 73244475

