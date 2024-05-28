{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in rec {
        formatter = pkgs.nixfmt;

        packages.raytracing = pkgs.haskellPackages.developPackage {
          root = ./.;
          returnShellEnv = true;
          withHoogle = false;
        };

        devShells.default = packages.raytracing.overrideAttrs (old: {
          nativeBuildInputs = old.nativeBuildInputs ++ (with pkgs; [
            editorconfig-checker
            nixfmt
            cabal-install
            hlint
            haskellPackages.fourmolu
          ]);
        });
      });
}
