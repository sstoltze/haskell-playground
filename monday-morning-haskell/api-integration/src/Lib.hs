module Lib where

import System.Environment
import Twilio
import Twilio.Messages

fetchSid :: IO String
fetchSid = getEnv "TWILIO_ACCOUNT_SID"

fetchToken :: IO String
fetchToken = getEnv "TWILIO_AUTH_TOKEN"

fetchNumber :: IO String
fetchNumber = getEnv "TWILIO_NUMBER"

sendMessage :: IO ()
sendMessage = do
  twilioNumber <- fetchNumber
  runTwilio' fetchSid fetchToken $ do
