module Main where

import Control.Monad (unless)
import Ext.Logger.Colog (setLineBuffering)
import Lib (runMigrationsAndServer)
import qualified System.Directory as FS

main :: IO ()
main = setLineBuffering >> checkLocalConfig >> runMigrationsAndServer

checkLocalConfig :: IO ()
checkLocalConfig = do
  let cfgPath = "./config/local.conf"
  cfgExists <- FS.doesFileExist cfgPath
  unless cfgExists $ do
    configTemplate <- readFile "./config/template.conf"
    writeFile cfgPath configTemplate
  let envPath = "./.env"
  envExists <- FS.doesFileExist envPath
  unless envExists $ do
    envTemplate <- readFile "./.env.template"
    writeFile envPath envTemplate
