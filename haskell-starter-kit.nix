{ mkDerivation, aeson, base, basement, bytestring, co-log
, configurator, containers, cryptonite, directory, esqueleto, hpack
, hspec, hspec-core, hspec-wai, http-client, http-client-tls
, http-types, jose, lib, memory, monad-logger, mtl, persistent
, persistent-postgresql, persistent-template, postgresql-simple
, random, resource-pool, resourcet, safe-exceptions, servant-auth
, servant-auth-server, servant-client, servant-foreign
, servant-options, servant-server, stm, tasty, tasty-hspec, text
, time, typerep-map, unliftio, unliftio-core, wai, wai-cors
, wai-extra, wai-logger, warp
}:
mkDerivation {
  pname = "haskell-starter-kit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base basement bytestring co-log configurator containers
    cryptonite directory esqueleto http-client http-client-tls jose
    memory monad-logger mtl persistent persistent-postgresql
    persistent-template postgresql-simple random resource-pool
    resourcet safe-exceptions servant-auth servant-auth-server
    servant-client servant-foreign servant-options servant-server stm
    text time typerep-map unliftio unliftio-core wai wai-cors
    wai-logger warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base basement bytestring co-log configurator containers
    cryptonite directory esqueleto http-client http-client-tls jose
    memory monad-logger mtl persistent persistent-postgresql
    persistent-template postgresql-simple random resource-pool
    resourcet safe-exceptions servant-auth servant-auth-server
    servant-client servant-foreign servant-options servant-server stm
    text time typerep-map unliftio unliftio-core wai wai-cors
    wai-logger warp
  ];
  testHaskellDepends = [
    aeson base basement bytestring co-log configurator containers
    cryptonite directory esqueleto hspec hspec-core hspec-wai
    http-client http-client-tls http-types jose memory monad-logger mtl
    persistent persistent-postgresql persistent-template
    postgresql-simple random resource-pool resourcet safe-exceptions
    servant-auth servant-auth-server servant-client servant-foreign
    servant-options servant-server stm tasty tasty-hspec text time
    typerep-map unliftio unliftio-core wai wai-cors wai-extra
    wai-logger warp
  ];
  prePatch = "hpack";
  homepage = "https://github.com/fullstack-development/haskell-starter-kit#readme";
  license = lib.licenses.bsd3;
}
