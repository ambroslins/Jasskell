module Lucid.Htmx where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Lucid.Base (Attributes, makeAttributes)

hxGet_ :: Text -> Attributes
hxGet_ = makeAttributes "hx-get"

hxPost_ :: Text -> Attributes
hxPost_ = makeAttributes "hx-post"

hxPatch_ :: Text -> Attributes
hxPatch_ = makeAttributes "hx-patch"

hxPut_ :: Text -> Attributes
hxPut_ = makeAttributes "hx-put"

hxDelete_ :: Text -> Attributes
hxDelete_ = makeAttributes "hx-delete"

hxOn_ :: Text -> Text -> Attributes
hxOn_ event = makeAttributes ("hx-on:" <> event)

hxPushUrl_ :: Text -> Attributes
hxPushUrl_ = makeAttributes "hx-push-url"

hxSelect_ :: Text -> Attributes
hxSelect_ = makeAttributes "hx-select"

hxSwap_ :: Text -> Attributes
hxSwap_ = makeAttributes "hx-swap"

hxSwapOob_ :: Text -> Attributes
hxSwapOob_ = makeAttributes "hx-swap-oob"

hxTarget_ :: Text -> Attributes
hxTarget_ = makeAttributes "hx-target"

hxTrigger_ :: Text -> Attributes
hxTrigger_ = makeAttributes "hx-trigger"

hxExt_ :: Text -> Attributes
hxExt_ = makeAttributes "hx-ext"

hxWsConnect_ :: Text -> Attributes
hxWsConnect_ = makeAttributes "hx-ws:connect"

hxVals_ :: (Aeson.ToJSON a) => a -> Attributes
hxVals_ = makeAttributes "hx-vals" . Text.decodeUtf8 . BS.toStrict . Aeson.encode

hxWsSend_ :: Attributes
hxWsSend_ = makeAttributes "hx-ws:send" ""
