module Main where

    import Prologue
    import Luna.Syntax.Text.Lexer.Runner
    import Data.Text32

    ------------------
    -- === Main === --
    ------------------
    
    main :: IO ()
    main = do
        inp <- getContents
        let txt = Data.Text32.fromList inp
        let parsed = evalDefLexer txt
        print parsed