{
  description = "experiments with syft";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        devPackages = with pkgs; [
          go            # Go compiler (latest stable, e.g. 1.22)
          gotools       # Go tools like goimports, godoc, etc.
          golangci-lint # Linter for Go
          delve         # Debugger for Go
          syft
        ];
      in {
        devShell = pkgs.mkShell {
          buildInputs = devPackages;
          shellHook = ''
            echo "Welcome to the Go development shell"
            export GOPATH=$HOME/go
            export PATH=$GOPATH/bin:$PATH
          '';
        };

      });
}
