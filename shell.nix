{ pkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let hspkgs = pkgs.haskell.packages.${compiler}.override {
     overrides = self: super: {
       quickwebapp = self.callPackage ./. {};
      };
   };
in
  hspkgs.quickwebapp.env
