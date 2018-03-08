{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prologue

import Control.Concurrent       (threadDelay)
import Luna.IR.Term.Basic       (passTest_run)
import System.Remote.Monitoring as EKG

sleep :: Int -> IO ()
sleep = threadDelay . (* 1e6)

main :: IO ()
main = do
    EKG.forkServer "localhost" 8888
    putStrLn "Running Luna pass test..."
    passTest_run
    sleep 5
    passTest_run
    sleep 8
    passTest_run
    sleep 13
    passTest_run
    sleep 21
    passTest_run
    sleep 34
    passTest_run
    putStrLn "Done!"
