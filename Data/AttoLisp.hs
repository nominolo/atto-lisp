{-# LANGUAGE OverloadedStrings, Rank2Types, DeriveDataTypeable, BangPatterns #-}
-- The following is for the ParseList stuff
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
             UndecidableInstances, ScopedTypeVariables, OverlappingInstances, EmptyDataDecls #-}
-- | Efficient parsing and serialisation of S-Expressions (as used by Lisp).
--
-- This module is intended to be imported qualified, e.g.:
--
-- > import qualified Data.AttoLisp as L
--
module Data.AttoLisp
  ( -- * Core Lisp Types
    Lisp(..), nil, isNull,
    -- * Type Conversion
    FromLisp(..), Result(..), fromLisp,
    Failure, Success, Parser,
    parse, parseMaybe, parseEither, typeMismatch,

    ToLisp(..), 

    -- * Constructors and destructors
    mkStruct,  struct,

    -- * Encoding and parsing
    encode, fromLispExpr,
    
    lisp, atom,
  )
where

import Blaze.ByteString.Builder.Char.Utf8 (fromChar)
import Blaze.ByteString.Builder.Word (fromWord8)
import Blaze.Text (double, integral)
import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Monad
import Data.Attoparsec.Char8 hiding ( Parser, Result, parse, string, double )
import Data.Data
import Data.Int  ( Int8, Int16, Int32, Int64 )
import Data.List ( foldl' )
import Data.Ratio ( Ratio )
import Data.Monoid
import Data.String
import Data.Word ( Word, Word8, Word16, Word32, Word64 )
import Numeric (showHex)
import qualified Data.Attoparsec as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Attoparsec.Zepto as Z
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import qualified Data.Map as M
-- | A Lisp expression (S-expression).
--
-- Symbols are case-sensitive.
data Lisp
  = Symbol T.Text   -- ^ A symbol (including keyword)
  | String T.Text   -- ^ A string.
  | Number Number   -- ^ A number
  | List [Lisp]     -- ^ A proper list: @(foo x 42)@
  | DotList [Lisp] Lisp  -- ^ A list with a non-nil tail: @(foo x
                         -- . 42)@.  The list argument must be
                         -- non-empty and the tail must be non-'nil'.
  deriving (Eq, Ord, Data, Typeable)

instance Show Lisp where
  showsPrec _ (Symbol a) = showString (T.unpack a)
  showsPrec _ (String t)  = shows (T.unpack t)
  showsPrec _ (Number n) = shows n
  showsPrec _ (List l) = showParen True (spaceSep l)
  showsPrec _ (DotList l d) =
    showParen True (spaceSep l . showString " . " . shows d)

spaceSep :: Show a => [a] -> ShowS
spaceSep []     = id
spaceSep (l1:ls1) = shows l1 . go1 ls1
 where
  go1 []     = id
  go1 (l:ls) = showChar ' ' . shows l . go1 ls

instance IsString Lisp where
  fromString s = String (fromString s)
  {-# INLINE fromString #-}

instance NFData Lisp where
  rnf (Symbol t) = rnf t
  rnf (String t)  = rnf t
  rnf (Number r)  = rnf r
  rnf (List l) = foldl' (\x y -> rnf y `seq` x) () l
  rnf (DotList l n) = foldl' (\x y -> rnf y `seq` x) () l `seq` rnf n

-- | Returns 'True' if the expression is @nil@ or the empty list.
isNull :: Lisp -> Bool
isNull (List []) = True
isNull (Symbol "nil") = True
isNull _ = False

-- | The empty list.
nil :: Lisp
nil = List []

-- | Failure continuation.
type Failure f r   = String -> f r
-- | Success continuation.
type Success a f r = a -> f r

-- | A continuation-based parser type.
newtype Parser a = Parser
  { runParser :: forall f r.
                  Failure f r
               -> Success a f r
               -> f r
  }

instance Monad Parser where
    m >>= g = Parser $ \kf ks -> let ks' a = runParser (g a) kf ks
                                  in runParser m kf ks'
    {-# INLINE (>>=) #-}
    return a = Parser $ \_kf ks -> ks a
    {-# INLINE return #-}
    fail msg = Parser $ \kf _ks -> kf msg
    {-# INLINE fail #-}

instance Functor Parser where
    fmap f m = Parser $ \kf ks -> let ks' a = ks (f a)
                                   in runParser m kf ks'
    {-# INLINE fmap #-}

instance Applicative Parser where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = apP
    {-# INLINE (<*>) #-}
    
instance Alternative Parser where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance MonadPlus Parser where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a b = Parser $ \kf ks -> let kf' _ = runParser b kf ks
                                    in runParser a kf' ks
    {-# INLINE mplus #-}

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apP #-}

-- | The result of running a 'Parser'.
data Result a = Error String
              | Success a
                deriving (Eq, Show, Typeable)

instance (NFData a) => NFData (Result a) where
    rnf (Success a) = rnf a
    rnf (Error err) = rnf err

instance Functor Result where
    fmap f (Success a) = Success (f a)
    fmap _ (Error err) = Error err
    {-# INLINE fmap #-}

instance Monad Result where
    return = Success
    {-# INLINE return #-}
    Success a >>= k = k a
    Error err >>= _ = Error err
    {-# INLINE (>>=) #-}

instance Applicative Result where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance MonadPlus Result where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a@(Success _) _ = a
    mplus _ b             = b
    {-# INLINE mplus #-}

instance Alternative Result where
    empty = mzero
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance Monoid (Result a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}


-- | Run a 'Parser'.
parse :: (a -> Parser b) -> a -> Result b
parse m v = runParser (m v) Error Success
{-# INLINE parse #-}

-- | Run a 'Parser' with a 'Maybe' result type.
parseMaybe :: (a -> Parser b) -> a -> Maybe b
parseMaybe m v = runParser (m v) (const Nothing) Just
{-# INLINE parseMaybe #-}

-- | Run a 'Parser' with an 'Either' result type.
parseEither :: (a -> Parser b) -> a -> Either String b
parseEither m v = runParser (m v) Left Right
{-# INLINE parseEither #-}

--test_parse001 =
--  parseMaybe 

--nth :: [Lisp] -> 

-- | Create a Lisp struct in a standardised format.
--
-- Fields in a struct are accessed by position.
mkStruct :: T.Text -> [Lisp] -> Lisp
mkStruct name fields = List (Symbol name : fields)

-- | A type that can be converted to an S-expression.
--
-- An example type and instance:
--
-- @data Coord { x :: Double, y :: Double }
--
-- instance ToLisp Coord where
--   toLisp (Coord x y) = 'struct' \"coord\" [toLisp x, toLisp y]
-- @
class ToLisp a where
  toLisp :: a -> Lisp

-- | A type that can be converted from an S-expression, with the
-- possibility of failure.
--
-- When writing an instance, use 'mzero' or 'fail' to make a
-- conversion fail, e.g. the value is of the wrong type.
--
-- An example type and instance:
--
-- @data Coord { x :: Double, y :: Double }
-- 
-- instance FromLisp Coord where
--   parseLisp ('DotList' [x] y) = pure (Coord x y) 
--   \-- A non-DotList value is of the wrong shape, so use mzero to fail.
--   parseLisp _          = 'mzero'
-- @
--
-- The above instance expects that @Coord 4 5@ is encoded as @(4
-- . 5)@.  This makes sense for a few special types, but most of the
-- time the standard encoding should be used: @(coord 4 5)@.  The
-- 'struct' combinator provides special support for this use case:
--
-- @instance FromLisp Coord where
--   parseLisp = 'struct' \"coord\" Coord
-- @
--
-- It uses some special type class magic to figure out the arity of
-- its second argument.
--
class FromLisp a where
  parseLisp :: Lisp -> Parser a

fromLisp :: FromLisp a => Lisp -> Result a
fromLisp = parse parseLisp

parseIntegral :: Integral a => Lisp -> Parser a
parseIntegral (Number n) = pure (floor n)
parseIntegral v          = typeMismatch "Integral" v
{-# INLINE parseIntegral #-}

-- | Fail parsing due to a type mismatch, with a descriptive message.
typeMismatch :: String -- ^ The name of the type you are trying to parse.
             -> Lisp  -- ^ The actual value encountered.
             -> Parser a
typeMismatch expected actual =
    fail $ "when expecting a " ++ expected ++ ", encountered " ++ name ++
           " instead"
  where
    name = case actual of
             Symbol _ -> "symbol"
             List []  -> "nil"
             List (Symbol s:_) -> T.unpack s ++ " object"
             List _   -> "list"
             DotList _ _ -> "list"
             String _ -> "string"
             Number _ -> "number"

class ParseList a b | a -> b where
  parseList :: String -> a -> [Lisp] -> Parser b

instance (IsFunction a f, ParseList' f a b) => ParseList a b where
  parseList = parseList' (undefined :: f)

class ParseList' f a b | f a -> b where
  parseList' :: f -> String -> a -> [Lisp] -> Parser b

instance (FromLisp a, IsFunction b f, ParseList' f b c, ParseList b c)
  => ParseList' HTrue (a -> b) c where
  parseList' _ msg _ []     = fail $ "Too few arguments for object: " ++ msg
  parseList' _ msg f (x:xs) = do
    y <- parseLisp x
    parseList msg (f y) xs

instance ParseList' HFalse a a where
  parseList' _ _msg r [] = return r
  parseList' _ msg  _ (_:_) = fail $ "Too many arguments for object: " ++ msg

data HTrue
data HFalse

class IsFunction a b | a -> b

instance TypeCast f HTrue => IsFunction (x -> y) f
instance TypeCast f HFalse => IsFunction a f

class TypeCast a b | a -> b, b -> a where
  typeCast :: a -> b

class TypeCast' t a b | t a -> b, t b -> a where
  typeCast' :: t -> a -> b

class TypeCast'' t a b | t a -> b, t b -> a where
  typeCast'' :: t -> a -> b

instance TypeCast' () a b => TypeCast a b where
  typeCast x = typeCast' () x

instance TypeCast'' t a b => TypeCast' t a b where
  typeCast' = typeCast''

instance TypeCast'' () a a where
  typeCast'' _ x  = x

-- | Decode structure serialised with 'mkStruct'.
--
-- The second argument should be a function, usually a constructor.
-- The resulting parser automatically figures out the arity of the
-- function.  For example:
--
-- @data Foo = Foo Int deriving (Eq, Show)
--
-- parseFoo :: Lisp -> 'Parser' Foo
--parseFoo = struct \"foo\" Foo
--
-- test = 'parseMaybe' parseFoo val == Just (Foo 23)
--  where val = 'List' ['Symbol' \"foo\", 'Number' 23]
-- @
--
struct :: ParseList f a => T.Text -> f -> Lisp -> Parser a
struct tag f (List (Symbol t:rest)) | t == tag =
  parseList (T.unpack tag) f rest
struct tag _ e = typeMismatch (T.unpack tag ++ " object") e

instance ToLisp Lisp where
  toLisp = id
  {-# INLINE toLisp #-}

instance FromLisp Lisp where
  parseLisp = pure
  {-# INLINE parseLisp #-}

instance ToLisp Bool where
  toLisp b = if b then Symbol "t" else nil
  {-# INLINE toLisp #-}

instance FromLisp Bool where
  parseLisp e = if isNull e then pure False else pure True
  {-# INLINE parseLisp #-}

instance ToLisp Char where
  toLisp c = String (T.singleton c)
  {-# INLINE toLisp #-}

instance FromLisp Char where
  parseLisp (String t)
    | T.compareLength t 1 == EQ = pure (T.head t)
  parseLisp e = typeMismatch "String" e
  {-# INLINE parseLisp #-}

instance ToLisp Integer where
  toLisp n = Number (fromInteger n)
  {-# INLINE toLisp #-}

instance FromLisp Integer where
  parseLisp = parseIntegral
  {-# INLINE parseLisp #-}

instance ToLisp Int where
  toLisp n = Number (fromIntegral n)
  {-# INLINE toLisp #-}

instance FromLisp Int where
  parseLisp = parseIntegral
  {-# INLINE parseLisp #-}

instance ToLisp T.Text where
  toLisp = String

instance FromLisp T.Text where
  parseLisp (String t) = pure t
  parseLisp e = typeMismatch "Text" e
  {-# INLINE parseLisp #-}

instance ToLisp () where
  toLisp () = List []
  {-# INLINE toLisp #-}

instance FromLisp () where
  parseLisp e | isNull e = pure ()
              | otherwise = typeMismatch "()" e
  {-# INLINE parseLisp #-}

instance ToLisp a => ToLisp (Maybe a) where
  toLisp Nothing = nil
  toLisp (Just a) = toLisp a
  {-# INLINE toLisp #-}

instance FromLisp a => FromLisp (Maybe a) where
  parseLisp e | isNull e = pure Nothing
  parseLisp e = Just <$> parseLisp e
  {-# INLINE parseLisp #-}

-- | No tag is used, hence type @a@ and @b@ must be different.
instance (ToLisp a, ToLisp b) => ToLisp (Either a b) where
  toLisp (Left a) = toLisp a
  toLisp (Right b) = toLisp b
  {-# INLINE toLisp #-}

-- | Tries to parse @a@ or, if that fails, parses a @b@.
instance (FromLisp a, FromLisp b) => FromLisp (Either a b) where
  parseLisp e = Left <$> parseLisp e <|> Right <$> parseLisp e
  {-# INLINE parseLisp #-}

instance ToLisp [Char] where
  toLisp s = String (T.pack s)
  {-# INLINE toLisp #-}

instance FromLisp [Char] where
  parseLisp (String t) = pure (T.unpack t)
  parseLisp e = typeMismatch "String" e
  {-# INLINE parseLisp #-}

instance ToLisp Double where
  toLisp = Number . D
  {-# INLINE toLisp #-}

instance FromLisp Double where
  parseLisp (Number n) =
    case n of
      D d -> pure d
      I i -> pure (fromIntegral i)
  parseLisp e | isNull e = pure (0/0)  -- useful?
  parseLisp e = typeMismatch "Double" e
  {-# INLINE parseLisp #-}

instance ToLisp Float where
  toLisp = Number . fromRational . toRational
  {-# INLINE toLisp #-}

instance FromLisp Float where
  parseLisp (Number n) =
    case n of
      D d -> pure (fromRational (toRational d))
      I i -> pure (fromIntegral i)
  parseLisp e | isNull e = pure (0/0)  -- useful?
  parseLisp e = typeMismatch "Float" e
  {-# INLINE parseLisp #-}

instance ToLisp Number where
  toLisp = Number
  {-# INLINE toLisp #-}

instance FromLisp Number where
  parseLisp (Number n) = pure n
  parseLisp e | isNull e = pure (D (0/0))  -- useful?
  parseLisp e = typeMismatch "Number" e
  {-# INLINE parseLisp #-}

instance ToLisp (Ratio Integer) where
  toLisp = Number . fromRational
  {-# INLINE toLisp #-}

instance FromLisp (Ratio Integer) where
  parseLisp (Number n) =
    case n of
      D d -> pure (toRational d)
      I i -> pure (fromIntegral i)
  parseLisp e = typeMismatch "Ratio Integer" e
  {-# INLINE parseLisp #-}

instance ToLisp Int8 where
  toLisp = Number . fromIntegral
  {-# INLINE toLisp #-}

instance FromLisp Int8 where
  parseLisp = parseIntegral
  {-# INLINE parseLisp #-}

instance ToLisp Int16 where
  toLisp = Number . fromIntegral
  {-# INLINE toLisp #-}

instance FromLisp Int16 where
  parseLisp = parseIntegral
  {-# INLINE parseLisp #-}

instance ToLisp Int32 where
  toLisp = Number . fromIntegral
  {-# INLINE toLisp #-}

instance FromLisp Int32 where
  parseLisp = parseIntegral
  {-# INLINE parseLisp #-}

instance ToLisp Int64 where
  toLisp = Number . fromIntegral
  {-# INLINE toLisp #-}

instance FromLisp Int64 where
  parseLisp = parseIntegral
  {-# INLINE parseLisp #-}

instance ToLisp Word where
  toLisp = Number . fromIntegral
  {-# INLINE toLisp #-}

instance FromLisp Word where
  parseLisp = parseIntegral
  {-# INLINE parseLisp #-}

instance ToLisp Word8 where
  toLisp = Number . fromIntegral
  {-# INLINE toLisp #-}

instance FromLisp Word8 where
  parseLisp = parseIntegral
  {-# INLINE parseLisp #-}

instance ToLisp Word16 where
  toLisp = Number . fromIntegral
  {-# INLINE toLisp #-}

instance FromLisp Word16 where
  parseLisp = parseIntegral
  {-# INLINE parseLisp #-}

instance ToLisp Word32 where
  toLisp = Number . fromIntegral
  {-# INLINE toLisp #-}

instance FromLisp Word32 where
  parseLisp = parseIntegral
  {-# INLINE parseLisp #-}

instance ToLisp Word64 where
  toLisp = Number . fromIntegral
  {-# INLINE toLisp #-}

instance FromLisp Word64 where
  parseLisp = parseIntegral
  {-# INLINE parseLisp #-}

instance ToLisp a => ToLisp [a] where
  toLisp l = List (map toLisp l)
  {-# INLINE toLisp #-}

instance FromLisp a => FromLisp [a] where
  parseLisp (List l) = mapM parseLisp l
  parseLisp e = typeMismatch "list" e
  {-# INLINE parseLisp #-}

instance (ToLisp a, ToLisp b) => ToLisp (a, b) where
  toLisp (a, b) = List [toLisp a, toLisp b]  -- TODO: could use dotted list
  {-# INLINE toLisp #-}

instance (FromLisp a, FromLisp b) => FromLisp (a, b) where
  parseLisp (List l) =
    case l of
      [a, b] -> (,) <$> parseLisp a <*> parseLisp b
      _ -> fail $ "Cannot unpack list into a pair"
  parseLisp (DotList hds b) =
    case hds of
      [a] -> (,) <$> parseLisp a <*> parseLisp b
      _ -> fail $ "Cannot unpack dotted list into a pair"
  parseLisp e = typeMismatch "pair" e
  {-# INLINE parseLisp #-}

instance (ToLisp a, ToLisp b, ToLisp c) => ToLisp (a, b, c) where
  toLisp (a, b, c) = List [toLisp a, toLisp b, toLisp c]
  {-# INLINE toLisp #-}

instance (FromLisp a, FromLisp b, FromLisp c) => FromLisp (a, b, c) where
  parseLisp (List l) =
    case l of
      [a, b, c] -> (,,) <$> parseLisp a <*> parseLisp b <*> parseLisp c
      _ -> fail $ "Cannot unpack list into a 3-tuple"
  parseLisp e = typeMismatch "3-tuple" e
  {-# INLINE parseLisp #-}

instance (ToLisp a, ToLisp b) => ToLisp (M.Map a b) where
  toLisp mp = toLisp [ (toLisp k, toLisp v) | (k,v) <- M.toList mp ]

instance (Ord a, FromLisp a, FromLisp b) => FromLisp (M.Map a b) where
  parseLisp e = M.fromList <$> parseLisp e

{-

We are using the standard Common Lisp read table.

The following characters are special:

  - whitespace: space, tab, newline, linefeed, return, page
  
  - terminating: ( ) , ` ' " ;

  - escaping: \  and  |

All remaining characters can be part of a symbol.  If a symbol looks
like an number then it is one.  Otherwise it's just a symbol.

-}

-- | Parse an arbitrary lisp expression.
lisp :: A.Parser Lisp
lisp = skipLispSpace *>
  (char '(' *> list_ <|>
   quoted <$> (char '\'' *> char '(' *> list_) <|>
   String <$> (char '"' *> lstring_) <|>
   atom)
 where
  quoted l = List [Symbol "quote", l]

-- | Parse a symbol or a number.  Symbols are expected to be utf8.
--
-- TODO: support escapes in symbols
atom :: A.Parser Lisp
atom = do
  sym <- takeWhile1 (\c -> not (terminatingChar c))
  -- If it looks like a number it is parsed as a number.
  let !w = B.unsafeIndex sym 0
  if (w >= 48 && w <= 57) ||  -- digit
     w == 43 || w == 45       -- '+' or '-'
   then do
     case A.parseOnly number sym of
       Left _  -> pure (Symbol (T.decodeUtf8 sym))
       Right n -> pure (Number n)
   else
     pure (Symbol (T.decodeUtf8 sym))
   
terminatingChar :: Char -> Bool
terminatingChar c =
  c == ',' || c == '(' || c == ')' || c == '\'' || c == ';' || c == '`' || isSpace c

list_ :: A.Parser Lisp
list_ = do
  skipLispSpace
  elems <- (lisp `sepBy` skipLispSpace) <* skipLispSpace <* char ')'
  return (List elems)

doubleQuote :: Word8
doubleQuote = 34
{-# INLINE doubleQuote #-}

backslash :: Word8
backslash = 92
{-# INLINE backslash #-}

skipLispSpace :: A.Parser ()
skipLispSpace = skipSpace >> optional comment >> skipSpace

comment :: A.Parser ()
comment = do
  _ <- char ';' >> A.many (notChar '\n')
  end <- atEnd
  if end then char '\n' >> return () else return ()

-- | Parse a string without a leading quote.
lstring_ :: A.Parser T.Text
lstring_ = {-# SCC "jstring_" #-} do
  s <- A.scan False $ \s c -> if s then Just False
                                   else if c == doubleQuote
                                        then Nothing
                                        else Just (c == backslash)
  _ <- A.word8 doubleQuote
  if backslash `B.elem` s
    then case Z.parse unescapeString s of
           Right r  -> return (T.decodeUtf8 r)
           Left err -> fail err
    else return (T.decodeUtf8 s)
{-# INLINE lstring_ #-}

unescapeString :: Z.Parser B.ByteString
unescapeString = Blaze.toByteString <$> go mempty where
  go acc = do
    h <- Z.takeWhile (/=backslash)
    let rest = do
          start <- Z.take 2
          let !slash = B.unsafeHead start
              !t = B.unsafeIndex start 1
              escape = case B.findIndex (==t) "\"\\/ntbrfu" of
                         Just i -> i
                         _      -> 255
          if slash /= backslash || escape == 255
            then fail "invalid JSON escape sequence"
            else do
            let cont m = go (acc `mappend` Blaze.fromByteString h `mappend` m)
                {-# INLINE cont #-}

            -- TODO: Handle Escapes \xNNNN  or \xNNNN ?
            cont (fromWord8 (B.unsafeIndex mapping escape))
    done <- Z.atEnd
    if done
      then return (acc `mappend` Blaze.fromByteString h)
      else rest
  mapping = "\"\\/\n\t\b\r\f"

fromLispExpr :: Lisp -> Blaze.Builder
fromLispExpr (String str) = string str
 where
   string s = fromChar '"' `mappend` quote s `mappend` fromChar '"'
   quote q =
     let (h, t) = T.break isEscape q in
     case T.uncons t of
       Just (c,t') -> Blaze.fromText h `mappend` escape c `mappend` quote t'
       Nothing     -> Blaze.fromText h
   isEscape c = c == '"' || c == '\\' || c < '\x20'
   escape '\"' = Blaze.fromByteString "\\\""
   escape '\\' = Blaze.fromByteString "\\\\"
   escape '\n' = Blaze.fromByteString "\\n"
   escape '\r' = Blaze.fromByteString "\\r"
   escape '\t' = Blaze.fromByteString "\\t"
   escape c
        | c < '\x20' = Blaze.fromString $ "\\x" ++ replicate (2 - length h) '0' ++ h
        | otherwise  = fromChar c
        where h = showHex (fromEnum c) "" 
fromLispExpr (Symbol t) = Blaze.fromText t
fromLispExpr (Number n) = fromNumber n
fromLispExpr (List []) = Blaze.fromByteString "nil"
fromLispExpr (List l) = enc_list l (fromChar ')')
fromLispExpr (DotList l t) =
  enc_list l (Blaze.fromByteString " . " `mappend` fromLispExpr t `mappend` fromChar ')')

enc_list :: [Lisp] -> Blaze.Builder -> Blaze.Builder
enc_list [] tl = fromChar '(' `mappend` tl
enc_list (x:xs) tl = fromChar '(' `mappend` fromLispExpr x `mappend` foldr f tl xs
 where f e t = fromChar ' ' `mappend` fromLispExpr e `mappend` t

fromNumber :: Number -> Blaze.Builder
fromNumber (I i) = integral i
fromNumber (D d) = double d

encode :: ToLisp a => a -> Lazy.ByteString
encode = Blaze.toLazyByteString . fromLispExpr . toLisp
{-# INLINE encode #-}
