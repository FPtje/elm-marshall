{
mkDerivation,
stdenv,

base,
aeson,
bytestring,
ghcjs-base,
ghcjs-dom,
ghcjs-ffiqq
}:
mkDerivation {
  pname = "elm-marshall";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [
    base
    aeson
    bytestring
    ghcjs-base
    ghcjs-dom
    ghcjs-ffiqq
  ];
  description = "Provide marshalling between Elm and ghcjs. Allows ghcjs to become Elm's IO monad.";
  license = stdenv.lib.licenses.mit;
}
