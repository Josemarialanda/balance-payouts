{
  description = "Simple haskell nix flake";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { flake-utils, nixpkgs, self }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config = {};
        overlays = [];
        pkgs = import nixpkgs { inherit config overlays system; };
      in {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [
          ];
          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            ghcid                   
            haskell-language-server 
            hlint                   
            ormolu                 
          ];
          withHoogle = true;
        };
      }
    );
}