{-# LANGUAGE OverloadedStrings, Rank2Types, DeriveDataTypeable, BangPatterns,
             MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
             UndecidableInstances #-}

module Main where

import Control.Applicative
import Data.AttoLisp
import qualified Data.Attoparsec as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework

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
           , tout :: Maybe Lisp
           }

main :: IO ()
main = defaultMain (map tcase tests)

tcase :: T -> Test.Framework.Test
tcase (T inp out) = testCase inpStr $ assertParse inpStr out out2
 where
  inpStr = BC.unpack inp
  out2 = A.parseOnly (lisp <* A.endOfInput) inp

assertParse _ Nothing (Left _) = return ()
assertParse _ Nothing (Right v2) = assertFailure $ "expected parse error\n but got: " ++ show v2
assertParse _ (Just v)  (Left e) = assertFailure $ "expected succesful parse: " ++ show v ++ "\n but got error: " ++ e
assertParse desc (Just v) (Right v2) = assertEqual desc v v2

tests = [ T "()" (Just $ List [])
        , T "42" (Just $ Number 42)
        , T ";;foo\n42" (Just $ Number 42)
        , T ";;foo\n;;bar\n42" (Just $ Number 42)
        , T "(4 5 6)" (Just $ List [Number 4, Number 5, Number 6])
        , T "(4 5 6 )" (Just $ List [Number 4, Number 5, Number 6])
        , T "(3 (4))" (Just $ List [Number 3, List [Number 4]])
        , T "'(3 4)" (Just $ List [Symbol "quote", List [Number 3, Number 4]])
        , T "\"a; however, b\"" (Just $ String "a; however, b")
        , T "(x ;comment\ny)" (Just $ List [Symbol "x", Symbol "y"])        , T "\"foo\"" (Just (String "foo"))
        , T "foo"     (Just (Symbol "foo"))
        , T "(foo \"bar\" 23)" (Just $ List [Symbol "foo", String "bar", Number 23])
        ]
