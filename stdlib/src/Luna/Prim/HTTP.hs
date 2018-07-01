{-# LANGUAGE OverloadedStrings #-}

module Luna.Prim.HTTP where

import           Prologue
import qualified Luna.IR as IR

import           Control.Arrow               ((***))
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as ByteString hiding (pack)
import qualified Data.ByteString.Char8       as ByteString
import qualified Data.CaseInsensitive        as CI
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Luna.Prim.Base              ()
import qualified Luna.Runtime as Luna

import qualified Network.HTTP.Client         as HTTP
import qualified Network.HTTP.Client.TLS     as HTTP
import qualified Network.HTTP.Simple         as HTTP
import qualified Network.HTTP.Types          as HTTP
import qualified Network.HTTP.Types.Header   as HTTP
import qualified Web.Authenticate.OAuth      as OAuth

import qualified Luna.Pass.Sourcing.Data.Def as Def
import           Luna.Std.Builder            (makeFunctionIO, makeFunctionPure, maybeLT, listLT, LTp (..), integer)


exports :: IO (Map IR.Name Def.Def)
exports = do
    let tupleT              = tuple2T textT textT
        oauthT              = maybeLT $ tuple4T textT textT textT textT
        maybeLTupleT        = maybeLT tupleT
        tupleListT          = listLT  tupleT
        tupleMaybeListT     = listLT $ tuple2T textT $ maybeLT textT
        textT               = LCons "Text"   []
        binaryT             = LCons "Binary" []
        tuple2T t1 t2       = LCons "Tuple2" [t1, t2]
        tuple4T t1 t2 t3 t4 = LCons "Tuple4" [t1, t2, t3, t4]

    let signOAuth1 :: (Text, Text, Text, Text) -> HTTP.Request -> IO HTTP.Request
        signOAuth1 (cak, cas, ot, ots) req = do
            let oauth = OAuth.newOAuth { OAuth.oauthConsumerKey = convert cak, OAuth.oauthConsumerSecret = convert cas }
                creds = OAuth.newCredential (convert ot) (convert ots)
            OAuth.signOAuth oauth creds req

    let primPerformHttpVal :: Text -> Text -> [(Text, Text)] -> Maybe (Text, Text) -> Maybe (Text, Text, Text, Text) -> [(Text, Maybe Text)] -> ByteString -> IO (HTTP.Response HTTP.BodyReader)
        primPerformHttpVal uri method headers auth oauth params body = do
            let packHeader (k, v) = (CI.mk $ convert k, convert v)
                packParam  (k, v) = (convert k, convert <$> v)
            baseReq <- HTTP.parseRequest (convert uri)
            let newHeaders = map packHeader headers
                oldHeaders = HTTP.requestHeaders baseReq
                oldParams  = HTTP.getRequestQueryString baseReq
            req <- baseReq
                    & HTTP.setRequestBodyLBS (convert body)
                    & HTTP.setRequestMethod  (convert method)
                    & HTTP.setRequestHeaders (oldHeaders <> newHeaders)
                    & HTTP.addRequestHeader  HTTP.hAccept (ByteString.pack "*/*")
                    & HTTP.setRequestQueryString (oldParams <> map packParam params)
                    & case auth of
                        Just (u, p) -> HTTP.setRequestBasicAuth (convert u) (convert p)
                        Nothing     -> id
                    & case oauth of
                        Just oauthData -> signOAuth1 oauthData
                        Nothing        -> return
            let managerSettings = if HTTP.secure req then HTTP.tlsManagerSettings else HTTP.defaultManagerSettings
            manager <- HTTP.newManager managerSettings
            HTTP.responseOpen req manager

    primPerformHttp <- makeFunctionIO (flip Luna.toValue primPerformHttpVal)
                                    [textT, textT, tupleListT, maybeLTupleT, oauthT, tupleMaybeListT, binaryT]
                                    (LCons "HttpResponse" [])

    let primUrlEncodeVal :: Text -> Text
        primUrlEncodeVal = convert . HTTP.urlEncode False . convert
    primUrlEncode <- makeFunctionPure (flip Luna.toValue primUrlEncodeVal) ["Text"] "Text"

    return $ Map.fromList [ ("primPerformHttp", primPerformHttp)
                          , ("primUrlEncode", primUrlEncode)
                          ]


type instance Luna.RuntimeRepOf (HTTP.Response b) = Luna.AsClass (HTTP.Response b) ('Luna.ClassRep "Std.HTTP" "HttpResponse")
instance Luna.ToValue b => Luna.ToObject (HTTP.Response b) where
    toConstructor imps v = Luna.Constructor "HttpResponse"
        [ Luna.toData imps . integer   $ HTTP.getResponseStatusCode v
        , Luna.toData imps $ (Map.fromList $ (convert . CI.original *** convert) <$> HTTP.responseHeaders v :: Map Text Text)
        , Luna.Thunk . Luna.toValue imps $ HTTP.responseBody v
        ]
