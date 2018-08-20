{-# LANGUAGE Strict #-}

module Main where

import Prologue

import qualified Luna.Shell.Command                as Command
import qualified Luna.Shell.Option                 as Option

-- TODO [Ara] Gracefully display errors (once TC integrated).

------------------
-- === Main === --
------------------

main :: IO ()
main = Command.runLuna =<< Option.execParser commandParser where
    commandParser = Option.info (Option.topLevel <**> Option.helper)
        (Option.fullDesc <> Option.progDesc
                            "The Luna Compiler command-line interface."
                         <> Option.header
                            "Visual and textual functional programming.")

