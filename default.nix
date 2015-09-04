{ mkDerivation, aeson, base, bytestring, containers, either, lucid
, servant, servant-lucid, servant-server, stdenv, text, warp
}:
mkDerivation {
  pname = "quickwebapp";
  version = "3.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers either lucid servant servant-lucid
    servant-server text warp
  ];
  description = "A quick webapp generator for any file processing tool";
  license = stdenv.lib.licenses.gpl3;
}
