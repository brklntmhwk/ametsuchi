# Copyright (C) 2025 Ohma Togaki
# SPDX-License-Identifier: MIT

{ inputs, lib, ... }:
let
  lib' = import ./lib {
    inherit inputs lib;
  };

  overlays = [
    inputs.emacs-overlay.overlays.emacs
  ];
in
{
  flake = {
    homeModules.twist = {
      imports = [
        inputs.twist.homeModules.emacs-twist
        (import ./modules/home-manager.nix lib'.mkEmacsConfig)
      ];
    };
  };

  perSystem =
    {
      config,
      emacs-config,
      pkgs,
      system,
      ...
    }:
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system overlays;
        # https://nixos.wiki/wiki/Unfree_Software
        config.allowUnfreePredicate =
          pkg:
          builtins.elem (lib.getName pkg) [
            # Add unfree packages that should be allowed to install here.
          ];
      };
      _module.args.emacs-config = lib'.mkEmacsConfig {
        inherit pkgs;
      };

      packages = {
        inherit emacs-config;

        # https://github.com/akirak/emacs-config/commit/cd81f077e64e836bb8b42cfa7f4228a48c189826
        emacsclient =
          inputs.nixpkgs.legacyPackages.${system}.runCommandLocal "emacsclient"
            { propagateBuildInputs = [ emacs-config.emacs ]; }
            ''
              mkdir -p $out/bin
              ln -t $out/bin -s ${emacs-config.emacs}/bin/emacsclient
            '';

        # TODO: add emacs envs with various emacs build versions
      };

      apps = emacs-config.makeApps { lockDirName = "lock"; };
    };
}
