{
  description = "MPF Service";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      mkOutputs = system:
        let
          pkgs = import nixpkgs { inherit system; };

          nodeApp = pkgs.buildNpmPackage {
            name = "mpf-service";
            src = ./.;
            npmDepsHash = "sha256-C1+n0AD5vE0Pe1BXBptMcaPUSkU54NG1NGBg2KDrrq4="; # pkgs.lib.fakeHash;
            buildPhase = "npm install";
            installPhase = ''
              mkdir -p $out
              cp -r . $out
            '';
          };
        in {
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              nodejs_22
              nodePackages.npm
            ];
          };

          packages.dockerImage = pkgs.dockerTools.buildImage {
            name = "mpf-service";
            tag = "0.0.1";

            copyToRoot = pkgs.buildEnv {
              name = "image-root";
              paths = [ nodeApp pkgs.nodejs_20 ];
              pathsToLink = [ "/bin" "/lib" ];
            };
            config = {
              Cmd = [
                "${pkgs.nodejs_20}/bin/node"
                "${nodeApp}/service.js"
              ];
              ExposedPorts = { "3000/tcp" = { }; };
              Env = [ "NODE_ENV=production" ];
            };

          };

        };
    in flake-utils.lib.eachDefaultSystem (mkOutputs);
}
