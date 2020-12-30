{ mkDerivation, base, containers, filepath, optparse-applicative
, semigroups, stdenv, text, yaml
}:
mkDerivation {
  pname = "turing-machine";
  version = "0.1.2.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers text yaml ];
  executableHaskellDepends = [
    base containers filepath optparse-applicative semigroups text yaml
  ];
  homepage = "https://gitlab.com/CrazyMind102/turing-machine#readme";
  license = stdenv.lib.licenses.bsd3;
}
