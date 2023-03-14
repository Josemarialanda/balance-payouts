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
        overlay = final: prev: {
          haskell = prev.haskell // {
            packageOverrides = hfinal: hprev:
              prev.haskell.packageOverrides hfinal hprev // {
                balance-payouts-interview-solution = hfinal.callCabal2nix "balance-payouts-interview-solution" ./. { };
              };
          };
          balance-payouts-interview-solution = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.balance-payouts-interview-solution;
        };
        config = {};
        overlays = [ overlay ];
        pkgs = import nixpkgs { inherit config overlays system; };
      in {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p.balance-payouts-interview-solution ];
          buildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.ghcid                   
            haskellPackages.haskell-language-server 
            haskellPackages.hlint                   
            haskellPackages.ormolu        
            pkgs.sqlite         
          ];
          withHoogle = true;
          # The shell removes any previous SQlite database present and creates a fresh database.
          shellHook = ''
            DB=./balancePayout.db
            if test -f "$DB"; then
              rm -f balancePayout.db
            fi
            echo "CREATE TABLE users (ut_userID INTEGER NOT NULL, ut_dob VARCHAR NOT NULL, ut_joinDob VARCHAR NOT NULL, ut_joinContribution DOUBLE NOT NULL, ut_monthlyContribution DOUBLE NOT NULL, ut_retirementDate VARCHAR NOT NULL, PRIMARY KEY ( ut_userID ));" > create.sql
            echo "CREATE TABLE balancePayout (bpt_date VARCHAR NOT NULL, bpt_balance DOUBLE NOT NULL, bpt_payout DOUBLE NOT NULL, bpt__ut_userID INTEGER NOT NULL);" >> create.sql
            sqlite3 balancePayout.db '.read create.sql'
            rm -f create.sql
            mkdir out
          '';
        };
        # 'nix build .' will build an executable in the result folder
        defaultPackage = pkgs.balance-payouts-interview-solution;
        # nix run .#exe runs the Haskell executable. Expects an empty database with name 'balancePayout.db' to be in path.
        # e.g 
        #  nix run .#exe -- --payout-rate 2 --max-age 120 --interest-path "./res/test_rates.csv" --in-path "./res/users.csv" --out-dir "./out" --payout-day 25 --contribution-day 1
        apps.exe = flake-utils.lib.mkApp { drv = pkgs.balance-payouts-interview-solution; };
      }
    );
}