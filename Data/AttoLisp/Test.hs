{-# LANGUAGE OverloadedStrings, Rank2Types, DeriveDataTypeable, BangPatterns,
             MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
             UndecidableInstances #-}

module Data.AttoLisp.Test where

import Control.Applicative
import Data.AttoLisp
import qualified Data.Attoparsec as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Test.HUnit

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

data T = T { tin  :: B.ByteString
           , tout :: Either String Lisp
           }

test_parse :: IO ()
test_parse = do
  mapM_ (\inp -> do
           let out = A.parseOnly (lisp <* A.endOfInput) (tin inp)
           putStrLn $ show (tin inp) ++ " => " ++ show out 
           assertEqual (show (tin inp)) (tout inp) out
        )
    inputs
 where
  inputs = [ T "()" (Right $ List [])
           , T "42" (Right $ Number 42)
           , T "(4 5 6)" (Right $ List [Number 4, Number 5, Number 6])
           , T "(3 (4))" (Right $ List [Number 3, List [Number 4]])
           , T "\"foo\"" (Right (String "foo"))
           , T "foo"     (Right (Symbol "foo"))
           , T "(foo \"bar\" 23)" (Right $ List [Symbol "foo", String "bar", Number 23])
           ]
