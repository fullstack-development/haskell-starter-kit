module Main where

import Ext.Logger.Colog (setLineBuffering)
import Lib (runMigrationsAndServer)

main :: IO ()
main = setLineBuffering >> runMigrationsAndServer
