{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, bytestring, containers, deepseq
      , dlist, fmlist, HUnit, QuickCheck, random, stdenv, text
      , utf8-string, vector
      }:
      mkDerivation {
        pname = "ListLike";
        version = "4.6.3";
        src = ./.;
        libraryHaskellDepends = [
          array base bytestring containers deepseq dlist fmlist text
          utf8-string vector
        ];
        testHaskellDepends = [
          array base bytestring containers dlist fmlist HUnit QuickCheck
          random text utf8-string vector
        ];
        homepage = "http://github.com/ddssff/listlike";
        description = "Generalized support for list-like structures";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
