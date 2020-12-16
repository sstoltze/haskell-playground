{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Demo where

import Project
import Reporting
import PrettyPrint

someProject :: Project () ProjectId
someProject = ProjectGroup "Sweden" () [stockholm, gothenburg, malmo]
  where
    stockholm  = Project "Stockholm" 1
    gothenburg = Project "Gothenburg" 2
    malmo      = ProjectGroup "Malmö" () [city, limhamn]
    city       = Project "Malmö City" 3
    limhamn    = Project "Limhamn" 4

reports :: IO (Project Report Report)
reports = calculateProjectReports someProject

ppProject :: IO ()
ppProject = reports >>= putStrLn . prettyProject prettyReport prettyReport
