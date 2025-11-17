{
  inputs = {
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    # ghc 9.2.8 packages
    nixpkgs.url = "github:nixos/nixpkgs/75a52265bda7fd25e06e3a67dee3f0354e73243c";
    classyplate.url = "github:eswar2001/classyplate/a360f56820df6ca5284091f318bcddcd3e065243";
    references.url = "github:eswar2001/references/120ae7826a7af01a527817952ad0c3f5ef08efd0";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ...}: {
      systems = import inputs.systems;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, system, ... }: {
        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
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
      };
    });
}
