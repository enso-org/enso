-- Allow ourselves to write Text literals.
{-# LANGUAGE OverloadedStrings #-}

module Main where

    import Prologue hiding (Symbol)
    import Luna.Syntax.Text.Lexer.Runner
    import Luna.Syntax.Text.Lexer.Token as T
    import Luna.Syntax.Text.Lexer.Symbol as S
    import Data.Text.Position            (Delta)
    import Data.Text32 hiding (convert)
    import Data.Aeson
    import qualified Data.ByteString.Lazy as B

    instance ToJSON Delta where
        toJSON delta = toJSON ((convert delta) :: Int)

    jsonStr :: String -> Value
    jsonStr = toJSON

    jsonT32 :: Text32 -> Value
    jsonT32 txt = toJSON ((convert txt) ::String)

    symbolWithValue :: String -> Text32 -> Value
    symbolWithValue name value = object 
        [ "symbol" .= toJSON name
        , "value" .= jsonT32 value ]

    symbolKw :: String -> Value
    symbolKw name = object 
        [ "symbol" .= jsonStr "Keyword"
        , "value" .= toJSON name ]

    instance ToJSON Symbol where
        toJSON :: Symbol -> Value
        -- TODO Block, Group, Marker
        toJSON (Var n) = symbolWithValue "Var" n
        toJSON (Cons v) = symbolWithValue "Cons" v
        toJSON KwCase = symbolKw "case"
        toJSON KwClass = symbolKw "class"
        toJSON KwDef = symbolKw "def"
        toJSON KwForeign = symbolKw "foreign"
        toJSON KwImport = symbolKw "import"
        toJSON KwNative = symbolKw "native"
        toJSON KwOf = symbolKw "of"
        toJSON (Operator v) = symbolWithValue "Operator" v
        toJSON (Modifier v) = symbolWithValue "Modifier" v
        toJSON (S.Number num) = object 
            [ "symbol" .= jsonStr "Number"
            , "value" .= jsonStr "TODO" ] -- TODO
        toJSON (Quote strtype bound) = object 
            [ "symbol" .= jsonStr "Quote"
            , "value" .= jsonStr "TODO" ] -- TODO
        toJSON (Str v) = symbolWithValue "Str" v
        toJSON (StrEsc esctype) = object 
            [ "symbol" .= jsonStr "StrEsc"
            , "value" .= jsonStr "TODO" ] -- TODO
        toJSON (List bound) = object 
            [ "symbol" .= jsonStr "List"
            , "value" .= jsonStr "TODO" ] -- TODO
        toJSON (Doc v) = symbolWithValue "Doc" v
        toJSON (Metadata v) = symbolWithValue "Metadata" v
        toJSON (Unknown v) = symbolWithValue "Unknown@Deprecated" v
        toJSON (Incorrect v) = symbolWithValue "Incorrect@Deprecated" v
        toJSON (Invalid invsymb) = object 
            [ "symbol" .= jsonStr "Invalid"
            , "value" .= jsonStr (show invsymb) ] -- TODO
        toJSON sym = object ["symbol" .= jsonStr (show sym)]

    instance ToJSON a => ToJSON (Token a) where
        toJSON token = object [
            "span" .= toJSON (token ^. T.span),
            "offset" .= toJSON (token ^. T.offset),
            "element" .= toJSON (token ^. T.element)
            ]


    main :: IO ()
    main = do
        inp <- getContents
        let txt = Data.Text32.fromList inp
        let parsed = evalDefLexer txt
        B.putStrLn (encode parsed)