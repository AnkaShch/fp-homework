{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE LambdaCase       #-}

module Block6.Parsers
  (
  Parser (..)
  , eof
  , ok
  , satisfy
  , element
  , stream
  , parseInt
  , parseCBS
  ) where

import Control.Applicative
import Control.Arrow (first)
import Data.Char

newtype Parser s a = Parser{runParser :: [s] -> Maybe (a, [s])}

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser parser) = Parser $ \s -> fmap (first f) (parser s)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure f = Parser $ \s -> Just (f, s)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) (Parser parseF) (Parser parseArg) = Parser $
    \s ->
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

ok :: Parser s ()
ok = Parser $ \s -> Just((), s)

eof :: Parser s ()
eof =
  Parser $ \case
    [] -> Just ((), [])
    _ -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy f =
  Parser $ \case
    [] -> Nothing
    (x:xs) ->
      if f x
      then Just (x, xs)
      else Nothing

element :: (Eq s) => s -> Parser s s
element a = satisfy (== a)

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

parseCBS :: Parser Char ()
parseCBS = parseCBS' <* eof
  where
    parseCBS' = element '(' *> parseCBS' <* element ')' <|> ok

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
