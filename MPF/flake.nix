{
  description = "MPF Aiken DevShell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    cardano-node-runtime = {
      url = "github:IntersectMBO/cardano-node?ref=10.1.4";
    };
    yaci-cli = {
      url =
        "https://github.com/bloxbean/yaci-devkit/releases/download/v0.10.5/yaci-cli-0.10.5-linux-X64.zip";
      flake = false;
    };

  };

  outputs = { self, nixpkgs, flake-utils, cardano-node-runtime, yaci-cli, ... }:
    let
      mkOutputs = system:
        let
          pkgs = import nixpkgs { inherit system; };
          node-pkgs = cardano-node-runtime.project.${system}.pkgs;
          cardano-node = node-pkgs.cardano-node;
          cardano-cli = node-pkgs.cardano-cli;
          cardano-submit-api = node-pkgs.cardano-submit-api;
        in {
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              nodejs
              aiken
              cardano-node
              cardano-cli
              cardano-submit-api
              just
              nodePackages.npm
              asciinema
            ];
            shellHook = ''

              mkdir -p ~/.yaci-cli/cardano-node/bin
              mkdir -p ~/.yaci-cli/bin/config
              chmod -R +w ~/.yaci-cli
              chmod -R +w ./config
              cp -R ${yaci-cli}/config .
              cp -R ${yaci-cli}/yaci-cli ~/.yaci-cli/bin
              export PATH=$PATH:~/.yaci-cli/bin
              cp ${cardano-node}/bin/cardano-node ~/.yaci-cli/cardano-node/bin/cardano-node
              cp ${cardano-cli}/bin/cardano-cli ~/.yaci-cli/cardano-node/bin
              cp ${cardano-submit-api}/bin/cardano-submit-api ~/.yaci-cli/cardano-node/bin
              chmod -R +w ~/.yaci-cli
              chmod -R +w ./config
            '';
          };

        };
    in flake-utils.lib.eachDefaultSystem (mkOutputs);
}
