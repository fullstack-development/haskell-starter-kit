module AppName.Auth.Commands where

import qualified Data.Text as T
import Servant.Auth.Server (writeKey)

createKey :: FilePath -> IO ()
createKey filePath = do
  writeKey filePath
  putStrLn $ "File is created at " <> filePath
