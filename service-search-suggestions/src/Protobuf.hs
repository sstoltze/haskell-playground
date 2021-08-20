module Protobuf where

import Data.Either (fromRight)
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ProtoLens (encodeMessage, decodeMessage, defMessage)
import Data.ProtoLens.Message (Message)

encodeProtobuf :: (Message m) => m -> BL.ByteString
encodeProtobuf = fromStrict . encodeMessage

decodeProtobuf :: (Message m) => BL.ByteString -> m
decodeProtobuf b = fromRight defMessage $ decodeMessage $ toStrict b
