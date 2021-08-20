module Protobuf where

import Data.Either (fromRight)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ProtoLens (encodeMessage, decodeMessage, defMessage)
import Data.ProtoLens.Message (Message)

encodeProtobuf :: (Message m) => m -> ByteString
encodeProtobuf = fromStrict . encodeMessage

decodeProtobuf :: (Message m) => ByteString -> Either String m
decodeProtobuf b = decodeMessage $ toStrict b

decodeProtobufWithDefault :: (Message m) => ByteString -> m
decodeProtobufWithDefault = fromRight defMessage . decodeProtobuf
