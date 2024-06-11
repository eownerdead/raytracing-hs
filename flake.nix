{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      utils,
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      rec {
        formatter = pkgs.nixfmt-rfc-style;

        packages.raytracing = pkgs.haskellPackages.developPackage {
          root = ./.;
          returnShellEnv = true;
          withHoogle = false;
        };

        devShells.default = packages.raytracing.overrideAttrs (old: {
          nativeBuildInputs =
            old.nativeBuildInputs
            ++ (with pkgs; [
              editorconfig-checker
              nixfmt-rfc-style
              cabal-install
              hlint
              haskellPackages.fourmolu
            ]);
        });
      }
    );
}
