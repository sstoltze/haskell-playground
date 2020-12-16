{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
-- https://mmhaskell.com/parsing-3
module MonadParser where

import Lib
-- Attoparsec uses a monad (Parser a), but these are still applicative so we can use <|>, *>, <*>, pure, etc.
import Control.Applicative
-- We can of couse also use >> instead of *>, return instead of pure, ...
import Data.Text (unpack, strip, Text)
import Data.Char
import Data.Attoparsec.Text

valueParser :: Parser Value
valueParser = nullParser
              <|> boolParser
              <|> numberParser
              <|> stringParser

-- Parse 'null' and if that succeeds then return ValueNull
nullParser :: Parser Value
nullParser = (string "null"
             <|> string "NULL"
             <|> string "Null")
             *> pure ValueNull

boolParser :: Parser Value
boolParser =
  trueParser *> pure (ValueBool True)
  <|> falseParser *> pure (ValueBool False)
  where
    trueParser = string "True" <|> string "true" <|> string "TRUE"
    falseParser = string "False" <|> string "false" <|> string "FALSE"

consumeLine :: Parser String
consumeLine = do
  str <- Data.Attoparsec.Text.takeWhile (/= '\n')
  char '\n'
  return (unpack str)

numberParser :: Parser Value
numberParser = ValueNumber <$> scientific

stringParser :: Parser Value
stringParser = (ValueString . unpack . strip) <$> takeTill (\c -> c == '|' || c == '\n')

nonNewlineSpace :: Char -> Bool
nonNewlineSpace c = c /= '\n' && isSpace c

barOrNewline :: Char -> Bool
barOrNewline c = c == '|' || c == '\n'

exampleLineParser :: Parser [Value]
exampleLineParser = do
  char '|'
  cells <- many cellParser
  char '\n'
  return cells
  where
    cellParser :: Parser Value
    cellParser = do
      skipWhile nonNewlineSpace
      val <- valueParser
      skipWhile (not . barOrNewline)
      char '|'
      return val

exampleColumnTitleLineParser :: Parser [String]
exampleColumnTitleLineParser = do
  char '|'
  keys <- many cellParser
  char '\n'
  return keys
  where
    cellParser :: Parser String
    cellParser = do
      skipWhile nonNewlineSpace
      val <- many letter
      skipWhile (not . barOrNewline)
      char '|'
      return val

exampleTableParser :: Parser ExampleTable
exampleTableParser = do
  string "Examples:"
  consumeLine
  keys <- exampleColumnTitleLineParser
  valueList <- many exampleLineParser
  return $ ExampleTable keys (map (zip keys) valueList)

insideBrackets :: Parser String
insideBrackets = do
  char '<'
  key <- many letter
  char '>'
  return key

nonBrackets :: Parser String
nonBrackets = many (satisfy (\c -> c /= '\n' && c /= '<'))

parseStatementLine :: Text -> Parser Statement
parseStatementLine signal = do
  string signal
  char ' '
  pairs <- many ((,) <$> nonBrackets <*> insideBrackets)
  finalString <- nonBrackets
  let (fullString, keys) = buildStatement pairs finalString
  return $ Statement fullString keys
  where
    buildStatement :: [(String, String)] -> String -> (String, [String])
    buildStatement [] final = (final, [])
    buildStatement ((str, key) :rest) final =
      let (str', keys) = buildStatement rest final
      in (str <> "<" <> key <> ">" <> str', key:keys)

parseStatement :: Parser Statement
parseStatement =  parseStatementLine "Given"
                  <|> parseStatementLine "When"
                  <|> parseStatementLine "Then"
                  <|> parseStatementLine "And"

scenarioParser :: Parser Scenario
scenarioParser = do
  string "Scenario: "
  title <- consumeLine
  statements <- many (parseStatement <* char '\n')
  examples <- exampleTableParser <|> return (ExampleTable [] [])
  return $ Scenario title statements examples

backgroundParser :: Parser Scenario
backgroundParser = do
  string "Background:"
  consumeLine
  statements <- many (parseStatement <* char '\n')
  examples <- exampleTableParser <|> return (ExampleTable [] [])
  return $ Scenario "Background" statements examples

featureParser :: Parser Feature
featureParser = do
  string "Feature: "
  title <- consumeLine
  (description, maybeBackground, scenarios) <- parseRestOfFeature
  return $ Feature title description maybeBackground scenarios
  where
    parseRestOfFeature = parseRestOfFeatureTail []
    parseRestOfFeatureTail prevDesc = do
      (fullDesc, maybeBG, scenarios) <- noDescriptionLine prevDesc <|> descriptionLine prevDesc
      return (fullDesc, maybeBG, scenarios)
    -- If next line is background, we are done with description.
    noDescriptionLine prevDesc = do
      maybeBackground <- optional backgroundParser
      scenarios <- some scenarioParser
      return (prevDesc, maybeBackground, scenarios)
    -- Otherwise add it to the description and try again
    descriptionLine prevDesc = do
      nextLine <- consumeLine
      parseRestOfFeatureTail (prevDesc ++ [nextLine])
