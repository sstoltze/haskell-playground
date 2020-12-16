-- https://mmhaskell.com/parsing-2
module ApplicativeParser where

import Lib
import Data.Char
import Data.Monoid ()
import Text.Regex.Applicative

parseFeatureFromFile :: FilePath -> IO Feature
parseFeatureFromFile inputFile = do
  fileContents <- lines <$> readFile inputFile
  let nonEmptyLines = filter (not . isEmpty) fileContents
  let trimmedLines = map trim nonEmptyLines
  let finalString = unlines trimmedLines
  undefined
  -- case parseFeatures finalString of
  --   _ -> undefined

isEmpty :: String -> Bool
isEmpty = all isSpace

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Simple parsers are created with sym :: a -> RE a a
parseLowercaseA :: RE Char Char
parseLowercaseA = sym 'a'

-- A parser of type RE s a takes a list of s'es and returns an a

-- Matching is done with match :: RE s a -> [s] -> Maybe a or it's infix =~
-- match parseLowercaseA "a" == Just 'a'
-- "a" =~ parseLowercaseA == Nothing
-- They must be able to match the entire list
-- match parseLowercaseA "ab" == Nothing

-- Match predicates with psym
parseNonNewline :: RE Char Char
parseNonNewline = psym (/= '\n')

-- Match full strings with string
readFeatureWord :: RE Char String
readFeatureWord = string "Feature: "

-- RE is applicative
readUntilEOL :: RE Char String
readUntilEOL = many parseNonNewline

-- And we can map things like data constructors over it
data TwoChars = TwoChars Char Char deriving Show

parseTwoChars :: RE Char TwoChars
parseTwoChars = TwoChars <$> parseNonNewline <*> parseNonNewline
-- match parseTwoChars "ab" == Just (TwoChars 'a' 'b')

-- We can also combine with <* and *> which matches both and discards the right, respective left, result
parseFirst :: RE Char Char
parseFirst = parseNonNewline <* parseNonNewline

parseSecond :: RE Char Char
parseSecond = parseNonNewline *> parseNonNewline

-- "ab" =~ parseFirst == Just 'a'
-- "ab" =~ parseSecond == Just 'b'
-- "a"  =~ parseFirst == Nothing

readThroughEOL :: RE Char String
readThroughEOL = readUntilEOL <* sym '\n'

readThroughBar :: RE Char String
readThroughBar = readUntilBar <* sym '|'

readUntilBar :: RE Char String
readUntilBar = many (psym (\c -> c /= '|' && c /= '\n'))

parseFeatureTitle :: RE Char String
parseFeatureTitle = readFeatureWord *> readThroughEOL

parseScenarioTitle :: RE Char String
parseScenarioTitle = string "Scenario: " *> readThroughEOL

-- RE implements Alternative to handle failures
parseEither :: RE Char String
parseEither = parseFeatureTitle <|> parseScenarioTitle -- Tries parseFeatureTitle, if that fails return parseScenarioTitle

valueParser :: RE Char Value
valueParser = nullParser
            <|> boolParser
            <|> numberParser
            <|> stringParser

-- Parse 'null' and if that succeeds then return ValueNull
nullParser :: RE Char Value
nullParser = (string "null"
             <|> string "NULL"
             <|> string "Null")
             *> pure ValueNull

boolParser :: RE Char Value
boolParser =
  trueParser *> pure (ValueBool True)
  <|> falseParser *> pure (ValueBool False)
  where
    trueParser = string "True" <|> string "true" <|> string "TRUE"
    falseParser = string "False" <|> string "false" <|> string "FALSE"

numberParser :: RE Char Value
-- If we put integerParser before decimalParser we would throw away decimals or fail
numberParser = (ValueNumber . read) <$> (negativeParser <|> decimalParser <|> integerParser)
  where
    -- some is like many, except it requires at least one match
    integerParser = some (psym isNumber)
    decimalParser = combineDecimal <$> many (psym isNumber) <*> sym '.' <*> integerParser
    negativeParser = (:) <$> sym '-' <*> (decimalParser <|> integerParser)
    combineDecimal base point decimal = base ++ (point : decimal)

stringParser :: RE Char Value
stringParser = (ValueString . trim) <$> readUntilBar

-- Now we can parse Values, lets learn to parse example tables
exampleLineParser :: RE Char [Value]
exampleLineParser = sym '|' *> many cellParser <* readThroughEOL
  where
    cellParser = many isNonNewlineSpace *> valueParser <* readThroughBar

isNonNewlineSpace :: RE Char Char
isNonNewlineSpace = psym (\c -> isSpace c && c /= '\n')

exampleColumnTitleLineParser :: RE Char [String]
exampleColumnTitleLineParser = sym '|' *> many cellParser <* readThroughEOL
  where
    cellParser = many isNonNewlineSpace *> many (psym isAlpha) <* readThroughBar

exampleTableParser :: RE Char ExampleTable
exampleTableParser = buildExampleTable <$> (string "Examples:" *> readThroughEOL *> exampleColumnTitleLineParser) <*> many exampleLineParser
  where
    buildExampleTable :: [String] -> [[Value]] -> ExampleTable
    buildExampleTable keys valueLists = ExampleTable keys (map (zip keys) valueLists)

-- Statements
parseStatementLine :: String -> RE Char Statement
parseStatementLine signal =
  string signal *> sym ' ' *> (finalizeStatement <$> (buildStatement <$> many ((,) <$> nonBrackets <*> insideBrackets) <*> nonBrackets))
  where
    buildStatement :: [(String, String)] -> String -> (String, [String])
    buildStatement [] lastStatement = (lastStatement, [])
    buildStatement ((str, key) : rest) keys =
      let (str', keys') = buildStatement rest keys
      in (str <> "<" <> key <> ">" <> str', key:keys')
    finalizeStatement :: (String, [String]) -> Statement
    finalizeStatement (regex, vars) = Statement regex vars

statementParser :: RE Char Statement
statementParser =     parseStatementLine "Given"
                  <|> parseStatementLine "When"
                  <|> parseStatementLine "Then"
                  <|> parseStatementLine "And"

nonBrackets :: RE Char String
nonBrackets = many (psym (\c -> c/= '\n' && c /= '<'))

insideBrackets :: RE Char String
insideBrackets = sym '<' *> many (psym (\c -> c /= '>')) <* sym '>'

scenarioParser :: RE Char Scenario
scenarioParser = Scenario <$>
  -- A scenario is a title
  (string "Scenario: " *> readThroughEOL) <*>
  -- Followed by many statements, one line for each
  many (statementParser <* sym '\n') <*>
  -- Followed by an optional table of examples
  (exampleTableParser <|> pure (ExampleTable [] []))

backgroundParser :: RE Char Scenario
backgroundParser = Scenario "Background" <$>
  -- For backgrounds there is no title so we throw the first line away
  ((string "Background:" *> readThroughEOL) *> many (statementParser <* sym '\n')) <*>
  (exampleTableParser <|> pure (ExampleTable [] []))

featureParser :: RE Char Feature
featureParser = Feature <$>
  (string "Feature: " *> readThroughEOL) <*>
  -- Ignore descriptions since applicative parsers have a hard time handling the lack of structure
  (pure []) <*>
  (optional backgroundParser) <*>
  (many scenarioParser)
