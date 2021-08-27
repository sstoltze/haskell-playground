{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
module Protobuf (encodeProtobuf,
                 decodeProtobuf,
                 decodeProtobufWithDefault,
                 buildInternalError,
                 buildInvalidRequestError,
                 buildSubmitSearchQueryRequest,
                 buildSubmitSearchQueryResponse,
                 buildGetSearchSuggestionsRequest,
                 buildGetSearchSuggestionsResponse) where

import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Either                (fromRight)
import           Data.ProtoLens             (decodeMessage, defMessage,
                                             encodeMessage)
import           Data.ProtoLens.Field       (HasField)
import           Data.ProtoLens.Message     (Message)
import qualified Data.Text                  as T (Text)
import           GHC.Word                   (Word32)
import           Lens.Micro
import qualified Proto.Search               as Search
import qualified Proto.Search_Fields        as Search

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

buildInvalidRequestError :: (Message a, HasField a "maybe'invalidRequestError" (Maybe Search.InvalidRequestError)) => T.Text -> a
buildInvalidRequestError err =
  defMessage
  & (Search.maybe'invalidRequestError ?~ invalid)
  where
    invalid :: Search.InvalidRequestError
    invalid =
      defMessage
      & Search.error .~ err

buildSubmitSearchQueryResponse :: Search.SubmitSearchQueryResponse
buildSubmitSearchQueryResponse =
  defMessage
  & (Search.maybe'success ?~ success)
  where
    success :: Search.SubmitSearchQueryResponse'Success
    success = defMessage

buildSubmitSearchQueryRequest :: T.Text -> Search.SubmitSearchQueryRequest
buildSubmitSearchQueryRequest query =
  defMessage
  & Search.query .~ query

buildGetSearchSuggestionsResponse :: [T.Text] -> Search.GetSearchSuggestionsResponse
buildGetSearchSuggestionsResponse r =
  defMessage
  & (Search.maybe'success ?~ success)
  where
    success :: Search.GetSearchSuggestionsResponse'Success
    success =
      defMessage
      & Search.result .~ r

buildGetSearchSuggestionsRequest :: T.Text -> Word32 -> Bool -> Search.GetSearchSuggestionsRequest
buildGetSearchSuggestionsRequest query limit isSafe =
  defMessage
  & Search.query .~ query
  & Search.limit .~ limit
  & Search.isSafe .~ isSafe
