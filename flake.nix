{
  description = "Nix Store GC Helper";

  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    nixpkgs.url = github:NixOS/nixpkgs;
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPackages = pkgs.haskell.packages.ghc924;
    in
    {
      packages = {
        nix-store-gc-extra = haskellPackages.callCabal2nix "nix-store-gc-extra" ./. { };
      };

      defaultPackage = self.packages.${system}.nix-store-gc-extra;

      devShell = pkgs.mkShell {
        packages =
          # Haskell tooling
          (with haskellPackages; [
            (haskellPackages.ghcWithPackages (_:
              let
                nix-store-gc-extra = self.packages.${system}.nix-store-gc-extra;
              in
              nix-store-gc-extra.propagatedBuildInputs ++ nix-store-gc-extra.buildInputs
            ))
            cabal-install
            haskell-language-server
            hlint
            fourmolu
          ])
          ++
          # Nix tooling
          (with pkgs; [
            nixpkgs-fmt
            rnix-lsp
          ]);
      };
    });
}
