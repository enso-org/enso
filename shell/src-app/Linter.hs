-- Allow ourselves to write Text literals.
{-# LANGUAGE OverloadedStrings #-}

module Main where

    import Prologue hiding (Symbol)
    import Luna.Syntax.Text.Lexer.Runner
    import Luna.Syntax.Text.Lexer.Token as T
    import Luna.Syntax.Text.Lexer.Symbol as S
    import Data.Text.Position            (Delta)
    import Data.Text32 hiding (convert, map)
    import Data.Aeson
    import qualified Data.ByteString.Lazy as B

    instance ToJSON Delta where
        toJSON delta = toJSON ((convert delta) :: Int)

    -- instance ToJSON Text32 where
    --     toJSON txt = toJSON ((convert txt) :: String)
    text32ToJSON :: Text32 -> Value
    text32ToJSON txt = toJSON ((convert txt) ::String)

    valueForSymbol :: Symbol -> Maybe Value
    -- TODO Block, Group, Marker
    valueForSymbol (Var n) = Just $ text32ToJSON n
    valueForSymbol (Cons v) = Just $ text32ToJSON v
    
    valueForSymbol KwCase = Just $ toJSON ("case" :: String)
    valueForSymbol KwClass = Just $ toJSON ("class" :: String)
    valueForSymbol KwDef = Just $ toJSON ("def" :: String)
    valueForSymbol KwForeign = Just $ toJSON ("foreign" :: String)
    valueForSymbol KwImport = Just $ toJSON ("import" :: String)
    valueForSymbol KwNative = Just $ toJSON ("native" :: String)
    valueForSymbol KwOf = Just $ toJSON ("of" :: String)

    valueForSymbol (Operator v) = Just $ text32ToJSON v
    valueForSymbol (Modifier v) = Just $ text32ToJSON v
    valueForSymbol (S.Number num) = Nothing -- TODO
    valueForSymbol (Quote strtype bound) = Nothing -- TODO
    valueForSymbol (Str v) = Just $ text32ToJSON v
    valueForSymbol (StrEsc esctype) = Nothing -- TODO
    valueForSymbol (List bound) = Nothing -- TODO
    valueForSymbol (Doc v) = Just $ text32ToJSON v

    valueForSymbol (Metadata v) = Just $ text32ToJSON v
    valueForSymbol (Unknown v) = Just $ text32ToJSON v -- deprecated
    valueForSymbol (Incorrect v) = Just $ text32ToJSON v -- deprecated
    valueForSymbol (Invalid invsymb) = Just $ toJSON ((show invsymb) :: String)

    valueForSymbol _ = Nothing

    instance ToJSON Symbol where
        toJSON :: Symbol -> Value
        toJSON sym = case valueForSymbol sym of
            Nothing -> object
                [ "symbol" .= text32ToJSON (showCons sym)
                , "tags" .= map text32ToJSON (getTags sym)
                ]
            Just v -> object
                [ "symbol" .= text32ToJSON (showCons sym)
                , "tags" .= map text32ToJSON (getTags sym)
                , "value" .= v 
                ]
        
    instance ToJSON a => ToJSON (Token a) where
        toJSON token = object 
            [ "span" .= toJSON (token ^. T.span)
            , "offset" .= toJSON (token ^. T.offset)
            , "element" .= toJSON (token ^. T.element)
            ]


    main :: IO ()
    main = do
        inp <- getContents
        let txt = Data.Text32.fromList inp
        let parsed = evalDefLexer txt
        B.putStrLn (encode parsed)