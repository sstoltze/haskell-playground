module Protobuf where

import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Either                (fromRight)
import           Data.ProtoLens             (decodeMessage, defMessage,
                                             encodeMessage)
import           Data.ProtoLens.Field       (HasField)
import           Data.ProtoLens.Message     (Message)
import qualified Data.Text                  as T (Text)
-- import           GHC.Word                   (Word32)
import           Lens.Micro

encodeProtobuf :: (Message m) => m -> ByteString
encodeProtobuf = fromStrict . encodeMessage

decodeProtobuf :: (Message m) => ByteString -> Either String m
decodeProtobuf b = decodeMessage $ toStrict b

decodeProtobufWithDefault :: (Message m) => ByteString -> m
decodeProtobufWithDefault = fromRight defMessage . decodeProtobuf
