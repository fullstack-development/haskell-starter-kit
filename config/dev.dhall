let LogLevel = <Debug | Info | Warning | Error>
let LogToFile = <NoLogToFile | AllowLogToFile : Text>

let dbConfig = {
  host = "localhost", 
  port = +5431, 
  database = "placid_db", 
  user = "placid_user", 
  password = "placid_pass", 
  poolLimit = +10
}

let logConfig = {
  appName = "AppName", 
  logToStdout = True, 
  logLevel = LogLevel.Debug, 
  logRawSql = True, 
  logToFile = LogToFile.AllowLogToFile "/tmp/haskell-starter-kit.log"
}

let authConfig = {
  pathToKey = "./keys/auth-key"
}

in
{
  dbConfig = dbConfig,
  logConfig = logConfig,
  authConfig = authConfig,
  appPort = +8080
}