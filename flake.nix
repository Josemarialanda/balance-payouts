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
          buildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.ghcid                   
            haskellPackages.haskell-language-server 
            haskellPackages.hlint                   
            haskellPackages.ormolu        
            pkgs.sqlite         
          ];
          withHoogle = true;
          shellHook = ''
            DB=./balancePayout.db
            if test -f "$DB"; then
              rm -f balancePayout.db
            fi
            echo "CREATE TABLE users (ut_userID INTEGER NOT NULL, ut_dob VARCHAR NOT NULL, ut_joinDob VARCHAR NOT NULL, ut_joinContribution DOUBLE NOT NULL, ut_monthlyContribution DOUBLE NOT NULL, ut_retirementDate VARCHAR NOT NULL, PRIMARY KEY ( ut_userID ));" > create.sql
            echo "CREATE TABLE balancePayout (bpt_date VARCHAR NOT NULL, bpt_balance DOUBLE NOT NULL, bpt_payout DOUBLE NOT NULL, bpt__ut_userID INTEGER NOT NULL);" >> create.sql
            sqlite3 balancePayout.db '.read create.sql'
            rm -f create.sql
          '';
        };
      }
    );
}