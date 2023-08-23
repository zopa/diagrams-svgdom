{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
, doBenchmark ? false
}:

let

  inherit (nixpkgs) pkgs;

  svg-builder  = { mkDerivation, base, blaze-builder, bytestring, hashable
                 , lib, text, unordered-containers
                 }:
      mkDerivation {
        pname = "svg-builder";
        version = "0.1.2";
        src = nixpkgs.fetchFromGitHub {
          owner = "zopa";
          repo = "svg-builder";
          rev = "97c8fd448cb0d5706d0a96706a6c7ab162e1a684";
          sha256 = "11qgd8zxi2czkkjffcv9x8rlfgm95krkgsj41hr74k75h1n3mvkf";
        };
        libraryHaskellDepends = [
          base blaze-builder bytestring hashable text unordered-containers
        ];
        homepage = "https://github.com/diagrams/svg-builder.git";
        description = "DSL for building SVG";
        license = lib.licenses.bsd3;
  };

  llib = import ./lib.nix {inherit pkgs;};

  f = { mkDerivation, base, base64-bytestring, bytestring, colour
      , containers, diagrams-core, diagrams-lib, filepath, ghcjs-dom
      , hashable, JuicyPixels, lens, lib, monoid-extras, mtl, semigroups
      , split, svg-builder, text, unordered-containers
      }:
      mkDerivation {
        pname = "diagrams-svgdom";
        version = "1.4.2";
        src = ./.;
        libraryHaskellDepends = [
          base base64-bytestring bytestring colour containers diagrams-core
          diagrams-lib filepath ghcjs-dom hashable JuicyPixels lens
          monoid-extras mtl semigroups split svg-builder text
          unordered-containers
        ];
        homepage = "http://projects.haskell.org/diagrams/";
        description = "Backend for diagrams that outputs ghcjs-dom transformations";
        license = lib.licenses.bsd3;
      };

  hp = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  hask = hp.override ({overrides = s: su : su // {
          svg-builder = s.callPackage svg-builder {};
  };});

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (hask.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
