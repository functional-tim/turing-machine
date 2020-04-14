{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, filepath
      , optparse-applicative, semigroups, stdenv, text, yaml
      }:
      mkDerivation {
        pname = "turing-machine";
        version = "0.1.2.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base containers text yaml ];
        executableHaskellDepends = [
          base containers filepath optparse-applicative semigroups text yaml
        ];
        testHaskellDepends = [ base containers text ];
        homepage = "https://gitlab.com/CrazyMind102/turing-machine#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
