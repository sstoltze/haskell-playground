{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Protobuf where

import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Either (fromRight)
import Data.ProtoLens (encodeMessage, decodeMessage, defMessage)
import Data.ProtoLens.Field (HasField)
import Data.ProtoLens.Message (Message)
import Data.Text (Text)

import Lens.Micro
import qualified Proto.Search as Search
import qualified Proto.Search_Fields as Search

encodeProtobuf :: (Message m) => m -> ByteString
encodeProtobuf = fromStrict . encodeMessage

decodeProtobuf :: (Message m) => ByteString -> Either String m
decodeProtobuf b = decodeMessage $ toStrict b

decodeProtobufWithDefault :: (Message m) => ByteString -> m
decodeProtobufWithDefault = fromRight defMessage . decodeProtobuf

buildInternalError :: (Message a, HasField a "maybe'internalError" (Maybe Search.InternalError)) => a
buildInternalError =
  defMessage
  & (Search.maybe'internalError ?~ defMessage)

buildInvalidRequestError :: (Message a, HasField a "maybe'invalidRequestError" (Maybe Search.InvalidRequestError)) => Text -> a
buildInvalidRequestError err =
  defMessage
  & (Search.maybe'invalidRequestError ?~ invalid)
  where
    invalid :: Search.InvalidRequestError
    invalid =
      defMessage
      & Search.error .~ err
