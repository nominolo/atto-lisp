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


data T = T { tin  :: B.ByteString
           , tout :: Maybe Lisp
           }

main :: IO ()
main = defaultMain
 [ testSimple
 , testTokens
 , testParseLisp
 , testShow
 ]

tcase :: T -> Test.Framework.Test
tcase (T inp out) = testCase inpStr $ assertParse inpStr out out2
 where
  inpStr = BC.unpack inp
  out2 = A.parseOnly (lisp <* A.endOfInput) inp

assertParse _ Nothing (Left _) = return ()
assertParse _ Nothing (Right v2) = assertFailure $ "expected parse error\n but got: " ++ show v2
assertParse _ (Just v)  (Left e) = assertFailure $ "expected succesful parse: " ++ show v ++ "\n but got error: " ++ e
assertParse desc (Just v) (Right v2) = assertEqual desc v v2

testSimple = testGroup "simple" $ map tcase
  [ T "()" (Just $ List [])
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
  -- this will have to do for now; no proper support/representation for escapes
  -- so the idea is that we try to return it completely unprocessed
  ]

testTokens = testGroup "tokens"
  [ testGroup "bare"    $ map tcase                   symbols
  , testGroup "KEYWORD" $ map (tcase . addPkg "" )    symbols
  , testGroup "intern"  $ map (tcase . addPkg "foo")  symbols
  , testGroup "extern"  $ map (tcase . addPkg "foo:") symbols
  , testGroup "intern-esc"  $ map (tcase . addPkg "|foo|")  symbols
  -- TODO: what's the right behaviour when we have package prefixes here?
  , tcase $ T "+1" (Just (Number 1))                  -- This is the integer 1,  not a symbol.
  ]

symbols =
        [ T "xyz\\," (Just (Symbol "xyz\\,")) -- ideally, something more like 'xyz',
        , T "\\n"    (Just (Symbol "\\n"))   -- NB: actually backslash-n, not newline!
                                              -- ideally, something more like 'n',
        , T "|foO|"  (Just (Symbol "|foO|")) -- ideally, something more like 'foO'
        , T "|f,o|"  (Just (Symbol "|f,o|")) -- ideally, something more like 'f,o'
        , T "|f\\|x|" (Just (Symbol "|f\\|x|")) -- ideally, something more like 'f|x'
        , T "|f\\|"   Nothing
        -- From the HyperSpec
        -- Note that instead of interpreting case, we aim here to preserve all the
        -- characters in the token (including the escapes)
        -- http://www.lispworks.com/documentation/HyperSpec/Body/02_cd.htm
        , T "FROBBOZ"  (Just (Symbol "FROBBOZ"))    -- The symbol whose name is FROBBOZ.
        , T "frobboz"  (Just (Symbol "frobboz"))    -- Another way to notate the same symbol.
        , T "fRObBoz"  (Just (Symbol "fRObBoz"))    -- Yet another way to notate it.
        , T "unwind-protect" (Just (Symbol "unwind-protect")) -- A symbol with a hyphen in its name.
        , T "+$" (Just (Symbol "+$"))               -- The symbol named +$.
        -- TODO: :-(; tricky!
        -- , T "1+" (Just (Symbol "1+"))               -- The symbol named 1+.
        , T "pascal_style" (Just (Symbol "pascal_style")) -- This symbol has an underscore in its name.
        , T "file.rel.43"  (Just (Symbol "file.rel.43"))  -- This symbol has periods in its name.
        , T "\\("  (Just (Symbol "\\("))            -- The symbol whose name is (.
        , T "\\+1" (Just (Symbol "\\+1"))           -- The symbol whose name is +1.
        , T "+\\1" (Just (Symbol "+\\1"))           -- Also the symbol whose name is +1.
        , T "\\frobboz" (Just (Symbol "\\frobboz")) -- The symbol whose name is fROBBOZ.
        -- TODO: :-( tricky!
        -- , T "3.14159265\\s0" (Just (Symbol "3.14159265\\s0"))   -- The symbol whose name is 3.14159265s0.
        -- , T "3.14159265\\S0" (Just (Symbol "3.14159265\\S0"))   -- A different symbol,  whose name is 3.14159265S0.
        -- , T "3.14159265s0" (Just (Symbol "3.14159265s0"))       -- A possible short float approximation to <PI>.
        , T "APL\\\\360" (Just (Symbol "APL\\\\360"))           -- The symbol whose name is APL\360.
        , T "apl\\\\360" (Just (Symbol "apl\\\\360"))           -- Also the symbol whose name is APL\360.
        , T "\\(b^2\\)\\-\\4*a*c" (Just (Symbol "\\(b^2\\)\\-\\4*a*c")) -- The name is (B^2) - 4*A*C.
                                                                        -- Parentheses and two spaces in it.
        , T "\\(\\b^2\\)\\-\\4*\\a*\\c" (Just (Symbol "\\(\\b^2\\)\\-\\4*\\a*\\c")) -- The name is (b^2) - 4*a*c.
                                                                                    -- Letters explicitly lowercase.
        , T "|\"|" (Just (Symbol "|\"|"))                       -- The same as writing \".
        , T "|(b^2) - 4*a*c|" (Just (Symbol "|(b^2) - 4*a*c|")) -- The name is (b^2) - 4*a*c.
        , T "|frobboz|" (Just (Symbol "|frobboz|"))             -- The name is frobboz,  not FROBBOZ.
        , T "|APL\\360|" (Just (Symbol "|APL\\360|"))           -- The name is APL360.
        , T "|APL\\\\360|" (Just (Symbol "|APL\\\\360|"))       -- The name is APL\360.
        , T "|apl\\\\360|" (Just (Symbol "|apl\\\\360|"))       -- The name is apl\360.
        , T "|\\|\\||"        (Just (Symbol "|\\|\\||"))        -- Same as \|\| ---the name is ||.
        , T "|(B^2) - 4*A*C|" (Just (Symbol "|(B^2) - 4*A*C|")) -- The name is (B^2) - 4*A*C.
                                                                -- Parentheses and two spaces in it.
        , T "|(b^2) - 4*a*c|" (Just (Symbol "|(b^2) - 4*a*c|")) -- The name is (b^2) - 4*a*c.
        ]

addPkg p t = t { tin  = BC.concat [ p, ":", tin t ]
               , tout = tweak (tout t)
               }
 where
  tweak Nothing           = Nothing
  tweak (Just (Symbol x)) = Just . Symbol $ T.concat [T.decodeUtf8 p, ":", x]
  tweak (Just x)          = Nothing

-- ----------------------------------------------------------------------
-- From Lisp to Haskell
-- ----------------------------------------------------------------------

testParseLisp = testGroup "parseLisp"
  [ tc "Maybe Just"   (Just (Just 3  :: Maybe Int)) "3"
  , tc "Maybe case 1" (Just (Nothing :: Maybe Int)) "nil"
  , tc "Maybe case 2" (Just (Nothing :: Maybe Int)) "NIL"
  , tc "Msg 1"        (Nothing :: Maybe Msg)        "(msg)"
  , tc "Msg 2"        (Nothing :: Maybe Msg)        "(msg bar baz)"
  , tc "Msg 3"        (Just (Msg "foo" 42))         "(msg \"foo\" 42)"
  ]
 where
  tc descr res inp =
    testCase msg $ assertParse "" res (parse inp)
   where
    msg = BC.unpack $ BC.concat [descr, " (", inp, ")" ]
  parse i = A.parseOnly (lisp <* A.endOfInput) i >>= parseEither parseLisp

-- ----------------------------------------------------------------------
-- Displaying Lisp
-- ----------------------------------------------------------------------

testShow = testGroup "show"
  [ tc "(42.2 foo \"blah\")" (List [Number 42.2, Symbol "foo", "blah"])
  , tc "(msg \"foo\" 42)"    (toLisp (Msg "foo" 42))
  ]
 where
  tc res inp = testCase res $ assertEqual "" res (show inp)
