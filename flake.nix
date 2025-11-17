{
  inputs = {
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    # ghc 9.2.8 packages
    nixpkgs.url = "github:nixos/nixpkgs/75a52265bda7fd25e06e3a67dee3f0354e73243c";
    classyplate.url = "github:eswar2001/classyplate/a360f56820df6ca5284091f318bcddcd3e065243";
    references.url = "github:eswar2001/references/120ae7826a7af01a527817952ad0c3f5ef08efd0";

    # ghc 8.10.7 packages
    ghc8-nixpkgs.url = "github:nixos/nixpkgs/43e3b6af08f29c4447a6073e3d5b86a4f45dd420";
    ghc8-classyplate.url = "github:Chaitanya-nair/classyplate/46f5e0e7073e1d047f70473bf3c75366a613bfeb";
    ghc8-classyplate.flake = false;
    ghc8-references.url = "github:eswar2001/references/35912f3cc72b67fa63a8d59d634401b79796469e";
    ghc8-references.flake = true;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ...}: {
      systems = import inputs.systems;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, system, ... }: {
        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.

        # GHC 8 support
        haskellProjects.ghc8 = {
          projectFlakeName = "cada";
          basePackages = inputs.ghc8-nixpkgs.legacyPackages.${system}.haskell.packages.ghc8107;
          imports = [
            inputs.ghc8-references.haskellFlakeProjectModules.output
          ];
          packages = {
            classyplate.source = inputs.ghc8-classyplate;
          };
          settings = {
            beam-core.jailbreak = true;
          };
          devShell = {
            mkShellArgs = {
              name = "ghc8-cada";
            };
            hlsCheck.enable = inputs.ghc8-nixpkgs.legacyPackages.${system}.stdenv.isDarwin; # On darwin, sandbox is disabled, so HLS can use the network.
          };
        };

        haskellProjects.default = {
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://community.flake.parts/haskell-flake/package-set
          # basePackages = pkgs.haskellPackages;

          # Extra package information. See https://community.flake.parts/haskell-flake/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          # defaults.enable = false;
          # devShell.tools = hp: with hp; {
          #   inherit cabal-install;
          #   inherit hp;
          # };
          projectFlakeName = "cada";
          # basePackages = pkgs.haskell.packages.ghc8107;
          basePackages = pkgs.haskell.packages.ghc92;
          imports = [
            inputs.references.haskellFlakeProjectModules.output
            inputs.classyplate.haskellFlakeProjectModules.output
          ];
          packages = {
          };
          settings = {
          };

          devShell = {
            # Enabled by default
            # enable = true;

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            # tools = hp: { fourmolu = null; ghcid = null; };
            mkShellArgs = {
              name = "cada";
            };
            hlsCheck.enable = pkgs.stdenv.isDarwin; # On darwin, sandbox is disabled, so HLS can use the network.
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.cada;

      };

      flake.haskellFlakeProjectModules = {
        # To use ghc 9 version, use
        # inputs.spider.haskellFlakeProjectModules.output

        # To use ghc 8 version, use
        # inputs.spider.haskellFlakeProjectModules.output-ghc8

        output-ghc9 = { pkgs, lib, ... }: withSystem pkgs.system ({ config, ... }:
            config.haskellProjects."default".defaults.projectModules.output
        );

        output-ghc8 = { pkgs, lib, ... }: withSystem pkgs.system ({ config, ... }:
            config.haskellProjects."ghc8".defaults.projectModules.output
        );
      };
    });
}