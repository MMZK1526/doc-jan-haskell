{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A very minimal testing framework.
--
-- I could have used hspec or HUnit, but I don't want to make the dependencies
-- keep growing, and it's fun to write this kind of stuff.
module Test where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Writer
import GHC.Generics
import System.Exit

newtype Test' a = Test' { unTest :: WriterT [String] (StateT String IO) a }
  deriving stock (Generic)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState String, MonadWriter [String])

type Test = Test' ()

runTest :: Test -> IO ()
runTest test = do
  result <- flip evalStateT "Test" . execWriterT $ unTest test
  case result of
    [] -> putStrLn "All tests passed."
    _  -> putStrLn (unlines result) >> exitFailure

(==.) :: Show a => Eq a => a -> a -> Test
actual ==. expected = unless (actual == expected) $ do
          name <- get
          tell [name ++ " failed:"
               , "  Expected:\n" ++ show expected
               , "  Actual:\n" ++ show actual ++ "\n" ]
infix 1 ==.

label :: String -> Test -> Test
label name test = do
  name' <- get
  put name
  test
  put name'

with :: (a -> b) -> (b -> b -> c) -> a -> a -> c
with f g x y = g (f x) (f y)
