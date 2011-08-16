{-# LANGUAGE OverloadedStrings, Rank2Types, DeriveDataTypeable, BangPatterns,
             MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
             UndecidableInstances #-}

module Data.AttoLisp.Test where

import Control.Applicative
import Data.AttoLisp
import qualified Data.Attoparsec as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data Msg = Msg T.Text Integer
  deriving (Eq, Show)

instance ToLisp Msg where
  toLisp (Msg t n) = mkStruct "msg" [toLisp t, toLisp n]

instance FromLisp Msg where
  parseLisp e = struct "msg" Msg e


test_sexp1 = 
  show (List [Number 42.2, Symbol "foo", "blah"]) == "(42.2 foo \"blah\")"

test_msg1 = toLisp (Msg "foo" 42)
test_msg2 = List [Symbol "msg"]
test_msg3 = List [Symbol "msg", "bar", "baz"]

test_parse :: IO ()
test_parse = do
  mapM_ (\inp ->
           putStrLn $ show inp ++ " => " ++ show (A.parseOnly (lisp <* A.endOfInput) inp))
    inputs
 where
  inputs = ["()", "42", "(4 5 6)", "(3 (4))", "(3(4))",
            "\"foo\"", "foo", "(foo \"bar\" 23)"]
