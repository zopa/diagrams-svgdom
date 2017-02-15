{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

with nixpkgs.haskell.lib;
let

  inherit (nixpkgs) pkgs;

  src = /home/bdoyle/diagrams-svgdom;

  f = { mkDerivation, base, base64-bytestring, bytestring, colour, containers,
      diagrams-core, diagrams-lib, diagrams-svg, filepath, ghcjs-dom, hashable,
      JuicyPixels, lens, monoid-extras, mtl, optparse-applicative, semigroups,
      split, stdenv, svg-builder, text
      }:
      mkDerivation {
        inherit src;
        pname = "diagrams-svgdom";
        version = "0.1";
        libraryHaskellDepends = [
          base base64-bytestring bytestring colour containers diagrams-core
          diagrams-lib diagrams-svg filepath ghcjs-dom hashable JuicyPixels lens
          monoid-extras mtl optparse-applicative semigroups split svg-builder
          text
        ];
        homepage = "http://projects.haskell.org/diagrams/";
        description = "SVG backend for diagrams drawing EDSL";
        license = stdenv.lib.licenses.bsd3;
      };

  ghcjs-dom-src = nixpkgs.srcOnly {
    name = "ghcjs-dom-src";
    src = nixpkgs.fetchFromGitHub {
      owner = "ghcjs";
      repo = "ghcjs-dom";
      rev = "579ebd3dd9bf7aa5a722ce0489dd0093ff6b3efe";
      sha256 = "0cg7dsna3jghibdn0g3iwm3ayqp5pxjwkamhphjl0kj1534dkv6h";
    };
  };

  svg-builder = nixpkgs.fetchFromGitHub {
    owner = "zopa";
    repo = "svg-builder";
    rev = "ff6d6c4a1f362c39d18688538edc9baab9409477";
    sha256 = "056h90xqcxkymi27s7zg6b9ffr2j8pljh1lbwbizclxrxalxwvfm";
  };

  addDepends = drv: xs:
    let old = drv.buildDepends or [];
    in old ++ xs;

  overrides = s: su: {
    diagrams-lib = overrideCabal su.diagrams-lib (drv: {
        version = "1.4.0.1";
        sha256  = "1iidlcqb001w2a0a4wnxv1qwybjv23qjx12k8a6hairhr28bah2i";
        buildDepends = addDepends drv [
          su.tasty-quickcheck
          su.numeric-extras
        ];
    });
    diagrams-core = overrideCabal su.diagrams-core (drv: {
      version = "1.4";
      sha256  = "1rrak6vym0q1c00cvhdlh29z8vsr6w81lq1xa9b61f5d7m42yl75";
    });

    diagrams-svg = overrideCabal su.diagrams-svg (drv: {
      version = "1.4.2";
      src = /home/bdoyle/diagrams-svg;
      # nixpkgs.fetchFromGitHub {
      #   owner = "zopa";
      #   repo = "diagrams-svg";
      #   rev = "a1036414de65ddaf1196661c48f4eb8fa903396b";
      #   sha256 = "1ijhnvpaid04rjam4plwv1ikg1v3n8cf63rz2zkr1fsjzbn2yhpn";
      # };
    });
    svg-builder = overrideCabal su.svg-builder (drv: {
      src = svg-builder;
    });
    optparse-applicative = overrideCabal su.optparse-applicative (drv: {
      version = "0.13.0.0";
      sha256  = "1b0c5fdq8bd070g24vrjrwlq979r8dk8mys6aji9hy1l9pcv3inf";
      buildDepends = with s; addDepends drv [ QuickCheck28 ];
      doCheck = false;
    });
    QuickCheck28 = overrideCabal su.QuickCheck (drv: {
      version = "2.8.2";
      sha256  = "1ai6k5v0bibaxq8xffcblc6rwmmk6gf8vjyd9p2h3y6vwbhlvilq";
    });
    ghcjs-dom-jsaddle = overrideCabal su.ghcjs-dom-jsaddle (drv: {
      src = "${ghcjs-dom-src}/ghcjs-dom-jsaddle";
      version = "0.7.0.3";
      sha256  = "1ai6k5v0bibaxq8xffcblc6rwmmk6gf8vjyd9p2h3y6vwbhlvilq";
    });
    ghcjs-dom = s.callPackage ({ mkDerivation, base, glib, gtk3, stdenv, text, transformers, webkitgtk3
                               , ghcjs-dom-jsaddle }: mkDerivation {
      pname = "ghcjs-dom";
      version = "0.7.0.4";
      sha256 = "1b2v67c0d9n0gl9z2cc496dj58jg60bh8skwr8m9q5qpvvkmg7hw";
      doHaddock = false;
      libraryHaskellDepends = [
        base text transformers
      ] ++ (if s.ghc.isGhcjs or false then with s; [
        ghcjs-prim ghc-prim ghcjs-base
      ] else [
        glib gtk3 webkitgtk3 ghcjs-dom-jsaddle
      ]);
      preConfigure = ''
        sed -i 's/\(transformers .*\)<0.5/\1<0.6/' *.cabal
      '';
      description = "DOM library that supports both GHCJS and WebKitGTK";
      license = stdenv.lib.licenses.mit;
    }) {};
  };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages.override {inherit overrides;}
                       else pkgs.haskell.packages.${compiler}.override {
                         inherit overrides;
                       };


  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
