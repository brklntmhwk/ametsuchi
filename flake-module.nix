# Copyright (C) 2025 Ohma Togaki
# SPDX-License-Identifier: MIT

{ inputs, lib, ... }:
let
  lib' = import ./lib {
    inherit inputs lib;
  };

  ob = inputs.org-babel.lib;
  initPath = ./init.org;
  initFile = lib.pipe initPath [
    builtins.readFile
    (ob.tangleOrgBabel { })
    (builtins.toFile "init.el")
  ];

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
      emacsConfig,
      pkgs,
      system,
      ...
    }:
    let
      emacsPackage = pkgs.emacs-git-pgtk.overrideAttrs (old: {
        src = pkgs.fetchFromGitHub {
          owner = "emacs-mirror";
          repo = "emacs";
          inherit (old.src) rev;
          sha256 = old.src.outputHash;
        };
      });
    in
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system overlays;
        # https://nixos.wiki/wiki/Unfree_Software
        config.allowUnfreePredicate =
          pkg:
          builtins.elem (lib.getName pkg) [
            # Add unfree package names that should be allowed to install here.
          ];
      };
      _module.args.emacsConfig = lib'.mkEmacsConfig {
        inherit pkgs emacsPackage initFile;
      };

      packages = {
        inherit emacsConfig;

        emacsclient =
          inputs.nixpkgs.legacyPackages.${system}.runCommandLocal "emacsclient"
            { propagateBuildInputs = [ emacsConfig.emacs ]; }
            ''
              mkdir -p $out/bin
              ln -t $out/bin -s ${emacsConfig.emacs}/bin/emacsclient
            '';
      };

      apps = emacsConfig.makeApps { lockDirName = "lock"; };
    };
}
