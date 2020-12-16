module Lib where

import Data.Scientific

data Feature = Feature
  { featureTitle :: String
  , featureDescription :: [String]
  , featureBackground :: Maybe Scenario
  , featureScenarios :: [Scenario]
  }

data Scenario = Scenario
  { scenarioTitle :: String
  , scenarioStatements :: [Statement]
  , scenarioExample :: ExampleTable
  }

-- This could/should probably be a HashMap
data ExampleTable = ExampleTable
  { exampleTableKeys :: [String]
  , exampleTableExamples :: [[(String, Value)]]
  }

data Value = ValueNumber Scientific
           | ValueString String
           | ValueBool Bool
           | ValueNull

data Statement = Statement
  { statementText :: String
  , statementExampleVariables :: [String]
  }


someFunc :: IO ()
someFunc = putStrLn testFunc

testFunc :: String
testFunc = "someFunc"
