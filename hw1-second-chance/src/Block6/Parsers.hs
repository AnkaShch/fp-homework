{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE LambdaCase       #-}

module Block6.Parsers
  (
  Parser (..)
  , element
  , eof
  , ok
  , parseCBS
  , parseInt
  , satisfy
  , stream
  ) where

import Control.Applicative
import Control.Arrow (first)
import Data.Char

-- | Type for Parser
--   return Maybe (parsed value, remaining part of the stream)
newtype Parser s a = Parser{runParser :: [s] -> Maybe (a, [s])}

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser parser) = Parser $ \s -> fmap (first f) (parser s)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure f = Parser $ \s -> Just (f, s)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) (Parser parseF) (Parser parseArg) =
    Parser $ \s ->
      case parseF s of
        Nothing -> Nothing
        Just (f, xs) ->
          case parseArg xs of
            Nothing         -> Nothing
            Just (arg, xs') -> Just (f arg, xs')

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser (const Nothing)

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (Parser left) <|> (Parser right) =
    Parser $ \s ->
      case left s of
        Nothing -> right s
        res     -> res

instance Monad (Parser s) where
  return :: a -> Parser s a
  return f = Parser $ \s -> Just (f, s)

  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (Parser parse) >>= f =
    Parser $ \s ->
      case parse s of
        Nothing      -> Nothing
        Just (a, xs) -> runParser (f a) xs

-- | Parser, which never crashes or absorbs input
ok :: Parser s ()
ok = Parser $ \s -> Just((), s)

-- | Checks that the parser has reached the end of the data stream (otherwise it crashes)
eof :: Parser s ()
eof =
  Parser $ \case
    [] -> Just ((), [])
    _ -> Nothing

-- | The parser accepts a predicate on a stream element, and returns the element, absorbing it from the stream
--   if the predicate on the element is True, otherwise it falls
satisfy :: (s -> Bool) -> Parser s s
satisfy f =
  Parser $ \case
    [] -> Nothing
    (x:xs) ->
      if f x
      then Just (x, xs)
      else Nothing

-- | Parse single stream element
element :: (Eq s) => s -> Parser s s
element a = satisfy (== a)

-- | Parse many stream elements
stream :: (Eq s) => [s] -> Parser s [s]
stream [] = return []
stream (x:xs) = resElm >>= resStream
  where
    resElm = element x
    resStream c =
      Parser $ \s ->
        case runParser (stream xs) s of
          Nothing       -> Nothing
          Just (r, xs') -> Just (c : r, xs')

-- | Parser of correct bracket sequences
--   falls if the sequence is incorrect, and doesn't fall if it is correct
parseCBS :: Parser Char ()
parseCBS = parseCBS' <* eof
  where
    parseCBS' = element '(' *> parseCBS' <* element ')' <* parseCBS' <|> ok

-- | Parse Integer that can be preceded by a + or -
--   return Just (parse number, remaining part of the stream) or Nothing? if process failed
parseInt :: Parser Char Int
parseInt = do
  sign <- stream "-" <|> stream "+" <|> stream ""
  makeInt sign <$> parseDigits
  where
    makeInt sign digits =
      foldl (\x y -> x * 10 + y) 0 digits *
      case sign of
        "-" -> -1
        _   -> 1

    parseDigits = do
      d <- parseDigit
      ds <- parseDigits <|> fmap (const []) ok
      return (d : ds)
      where
        parseDigit :: Parser Char Int
        parseDigit = fmap digitToInt (satisfy isDigit)
