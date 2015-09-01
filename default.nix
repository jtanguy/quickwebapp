{ mkDerivation, base, bytestring, http-types, scotty, stdenv, text
}:
mkDerivation {
  pname = "quickwebapp";
  version = "2.1.0.0";
  src = ./.;
  buildDepends = [ base bytestring http-types scotty text ];
  description = "A quick webapp generator for any file processing tool";
  license = stdenv.lib.licenses.gpl3;
}
