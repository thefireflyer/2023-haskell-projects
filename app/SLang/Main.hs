-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

module SLang.Main where

-------------------------------------------------------------------------------

import Control.Monad (void, (<=<))
import Data.Functor (($>))
import Data.HashMap.Lazy (HashMap)
import Data.Map qualified as Map
import GHC.Base (failIO, (<|>))
import Text.Parsec (ParseError, alphaNum, anyChar, anyToken, between, digit, endOfLine, many, many1, manyTill, noneOf, notFollowedBy, option, optionMaybe, optional, parse, sepBy, sepBy1, skipMany, skipMany1, string, (<?>))
import Text.Parsec.Char (char, endOfLine, lower, space, spaces, tab, upper)
import Text.ParserCombinators.Parsec (try)
import Turtle (filename)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

defaultArgs :: [String]
defaultArgs = ["/home/casey/dev/hsk/test0/appdata/sl/hello-world.0.1.0.sl"]

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

main :: [String] -> IO ()
main [path] = readSL path >>= print
main [] = main defaultArgs
main _ = putStrLn "invalid args"

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

data Program = Program
  { programID :: ProgramID,
    programTypes :: HashMap String (),
    programClasses :: HashMap String (),
    programFunctions :: HashMap String (),
    programSource :: ()
  }
  deriving (Show)

-------------------------------------------------------------------------------

data ProgramID = ProgramID
  { programName :: String,
    programMajor :: Int,
    programMinor :: Int,
    programPatch :: Int
  }
  deriving (Show)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- |
-- Reads the provided source file
--
-- Returns the SL program if successful
-- readSL :: FilePath -> IO Program
readSL path = do
  id <- tryParse parseFilename (filename path)
  raw <- readFile path
  tryParse parseSourceCode raw
  where
    -- return (Program id path)

    tryParse parser input = either (error . show) return (parser input)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

parseFilename :: String -> Either ParseError ProgramID
parseFilename = parse s "(unknown)"
  where
    s = do
      name <- many1 (alphaNum <|> char '-')
      _ <- char '.'
      major <- read <$> many1 digit
      _ <- char '.'
      minor <- read <$> many1 digit
      _ <- char '.'
      patch <- read <$> many1 digit
      _ <- string ".sl"
      return (ProgramID name major minor patch)

-------------------------------------------------------------------------------

parseSourceCode = parse s "(unknown)"
  where
    s = many (skipMany blankLine >> decl)

    decl = do
      docs <- many docComment
      x <- try declData <|> try declClass <|> declFunc
      return (docs, x)

    blankLine = whitespace >> (comment <|> void endOfLine)

    whitespace = skipMany (space <|> tab)
    whitespace1 = skipMany1 (space <|> tab)

    comment = string "--" >> manyTill anyChar (try endOfLine) >> return () <?> "comment"
    docComment = string "--|" >> manyTill anyChar (try endOfLine) <?> "doc comment"

    declData = do
      _ <- string "data "
      name <- pascalCase
      params <- many (try $ whitespace1 >> snakeCase)
      body <-
        option
          [(name, [])]
          ( try $
              do
                _ <- whitespace
                _ <- optional $ try (endOfLine >> (tab <|> (space >> space >> space)))
                _ <- whitespace
                _ <- char '='
                _ <- whitespace
                _ <- optional $ try (endOfLine >> (tab <|> (space >> space >> space)))

                try
                  ( do
                      _ <- whitespace
                      _ <- char '{'
                      _ <- whitespace
                      props <-
                        sepBy1
                          ( do
                              _ <- whitespace
                              left <- snakeCase
                              _ <- whitespace
                              _ <- char ':'
                              _ <- whitespace
                              right <- pascalCase
                              return (left, right)
                          )
                          endOfLine
                      _ <- whitespace
                      _ <- char '}'
                      return [(name, props)]
                  )
                  <|> try
                    ( do
                        inner <- many (noneOf "\r\n")
                        _ <- many anyToken
                        return [(name, [(inner, "")])]
                    )
          )
      return $ show (name, params, body)

    declClass = string "class " >> pascalCase

    declFunc = do
      sym <- snakeCase
      _ <- whitespace >> char '=' >> whitespace
      _ <- manyTill anyChar (try endOfLine)
      return sym

    snakeCase = do x <- lower; xs <- many (lower <|> digit <|> char '_'); return (x : xs)
    pascalCase = do x <- upper; xs <- many (alphaNum <|> char '_'); return (x : xs)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

runSL :: Program -> IO ()
runSL = print

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
