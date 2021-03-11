module Main where

import Ext.Logger.Colog (setLineBuffering)
import Lib (runMigrationsAndServer)
import qualified System.Directory as FS

main :: IO ()
main = setLineBuffering >> checkLocalConfig >> runMigrationsAndServer

checkLocalConfig :: IO ()
checkLocalConfig = do
  let cfgPath = "./config/local.conf"
  isExist <- FS.doesFileExist cfgPath
  if isExist then pure () else writeFile cfgPath ""
