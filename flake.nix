{
  description = "PureScript diff-compare project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    
    # Use easy-purescript-nix for PureScript tooling
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, easy-purescript-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        # Import easy-purescript-nix packages
        easy-ps = import easy-purescript-nix { inherit pkgs; };

        # Use PureScript 0.15.15 or newer
        # easy-purescript-nix provides the latest stable versions
        purs = easy-ps.purs-0_15_15 or easy-ps.purs;
        
        # Use Spago 0.93 or latest available
        # easy-purescript-nix regularly updates to latest spago versions
        spago = easy-ps.spago-next or easy-ps.spago;

        # Build the project
        buildInputs = [
          purs
          spago
          pkgs.nodejs
        ];

      in
      {
        # Development shell
        devShells.default = pkgs.mkShell {
          inherit buildInputs;
          
          shellHook = ''
            echo "╔════════════════════════════════════════════════╗"
            echo "║   PureScript Development Environment          ║"
            echo "╚════════════════════════════════════════════════╝"
            echo ""
            echo "PureScript version: $(purs --version)"
            echo "Spago version:      $(spago version)"
            echo "Node.js version:    $(node --version)"
            echo ""
            echo "Commands:"
            echo "  spago run       - Run the application"
            echo "  spago build     - Build the project"
            echo "  spago test      - Run tests"
            echo "  nix run         - Build and run via Nix"
            echo ""
          '';
        };

        # Default package - build the project
        packages.default = pkgs.stdenv.mkDerivation {
          name = "purescript-diff-compare";
          src = ./.;
          
          inherit buildInputs;
          
          buildPhase = ''
            export HOME=$TMPDIR
            
            # Install dependencies and build
            spago build
          '';
          
          installPhase = ''
            mkdir -p $out/bin
            mkdir -p $out/output
            
            # Copy built output
            cp -r output $out/
            
            # Create a run script
            cat > $out/bin/purescript-diff-compare <<EOF
            #!${pkgs.bash}/bin/bash
            exec ${pkgs.nodejs}/bin/node $out/output/Main/index.js "\$@"
            EOF
            chmod +x $out/bin/purescript-diff-compare
          '';
        };

        # Apps for 'nix run'
        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/purescript-diff-compare";
        };

        # Convenience app to run with spago directly
        apps.spago-run = {
          type = "app";
          program = toString (pkgs.writeShellScript "spago-run" ''
            export PATH="${pkgs.lib.makeBinPath buildInputs}:$PATH"
            cd ${./.}
            exec ${spago}/bin/spago run "$@"
          '');
        };
      }
    );
}
