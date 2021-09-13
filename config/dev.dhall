let LogSeverity = <Debug | Info | Warning | Error>
let LogToFile = <NoLogToFile | AllowLogToFile : Text>

let dbConfig = {
  host = "localhost", 
  port = +5431, 
  database = "haskell_starter-kit-db", 
  user = "haskell_starter-kit-user", 
  password = "haskell_starter-kit-pass", 
  poolLimit = +10
}

let loggerConfig = {
  appName = "Haskell Starter Kit", 
  logToStdout = True, 
  logLevel = LogSeverity.Debug, 
  logRawSql = True, 
  logToFile = LogToFile.AllowLogToFile "/tmp/haskell-starter-kit.log"
}

let authConfig = {
  pathToKey = "./keys/auth-key"
}

in
{
  dbConfig = dbConfig,
  loggerConfig = loggerConfig,
  authConfig = authConfig,
  appPort = +8080
}