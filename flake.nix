# Copyright (C) 2025 Ohma Togaki
# SPDX-License-Identifier: MIT

{
  description = ''
    Ametsuchi is the universe ── It encompasses (almost) everything that forms
    my ideal Emacs workstation, a living cosmos where creativity, logic,
    and intuition intertwine.
  '';

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://brklntmhwk.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "brklntmhwk.cachix.org-1:mGWjznSV6FglvHR7/2sa4MrCtGHMLiAOc9Ru+tEkdyg="
    ];
  };

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # Nix utils
    nix-filter.url = "github:numtide/nix-filter";

    # Dev
    git-hooks-nix = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Emacs-overlay
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    # Emacs-twist
    org-babel.url = "github:emacs-twist/org-babel";
    twist.url = "github:emacs-twist/twist.nix";
    twist-overrides.url = "github:emacs-twist/overrides";
    twist2elpa = {
      url = "github:emacs-twist/twist2elpa";
      inputs.twist.follows = "twist";
    };

    # Package registries for Emacs-twist
    melpa = {
      url = "github:melpa/melpa";
      flake = false;
    };
    gnu-elpa = {
      url = "github:elpa-mirrors/elpa";
      flake = false;
    };
    nongnu-elpa = {
      url = "github:elpa-mirrors/nongnu";
      flake = false;
    };
    gnu-elpa-archive = {
      url = "file+https://elpa.gnu.org/packages/archive-contents";
      flake = false;
    };
    nongnu-elpa-archive = {
      url = "file+https://elpa.nongnu.org/nongnu/archive-contents";
      flake = false;
    };

    # Miscellaneous
    tree-sitter-astro = {
      url = "github:virchau13/tree-sitter-astro";
      flake = false;
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      ...
    }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [
        "aarch64-linux"
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      lib' = import ./lib {
        inherit inputs;
        inherit (nixpkgs) lib;
      };
    in
    {
      homeManagerModules = rec {
        ametsuchi = import ./modules/home-manager.nix {
          inherit inputs;
          inherit (lib') mkEmacsConfig;
        };
        default = ametsuchi;
      };
      maidModules = rec {
        ametsuchi = import ./modules/nix-maid.nix lib'.mkEmacsConfig;
        default = ametsuchi;
      };

      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ inputs.emacs-overlay.overlays.emacs ];
            # https://nixos.wiki/wiki/Unfree_Software
            config.allowUnfreePredicate =
              pkg:
              builtins.elem (nixpkgs.lib.getName pkg) [
                # Explicitly add unfree packages here.
              ];
          };

          emacs-config = lib'.mkEmacsConfig {
            inherit pkgs;
          };

          emacs-config-no-pgtk = lib'.mkEmacsConfig {
            inherit pkgs;
            emacsPackage = inputs.emacs-overlay.packages.${system}.emacs-git;
          };
        in
        {
          inherit emacs-config emacs-config-no-pgtk;

          # https://github.com/akirak/emacs-config/commit/cd81f077e64e836bb8b42cfa7f4228a48c189826
          emacsclient =
            pkgs.runCommandLocal "emacsclient"
              {
                propagateBuildInputs = [ emacs-config.emacs ];
              }
              ''
                mkdir -p $out/bin
                ln -t $out/bin -s ${emacs-config.emacs}/bin/emacsclient
              '';

          # https://github.com/akirak/emacs-config/commit/111167fa21e0179ec54de7ee062a3d8164926cae
          elpa-archive = inputs.twist2elpa.lib.buildElpaArchiveAsTar {
            asInitDirectory = true;
            name = "elpa-archive-${builtins.substring 0 8 (self.lastModifiedDate)}"; # e.g., elpa-archive-19701231.tar
          } emacs-config;

          # https://github.com/akirak/emacs-config/commit/a6b5185ece1746b386c0c452605fa37d3fd30a54
          init-file = pkgs.runCommandLocal "init.el" { } ''
            for file in ${builtins.concatStringsSep " " emacs-config.initFiles}
            do
              cat "$file" >> "$out"
            done
          '';
        }
      );

      apps = forAllSystems (
        system:
        let
          # Re-import pkgs to ensure overlays are applied for app generation.
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ inputs.emacs-overlay.overlays.emacs ];
            config.allowUnfreePredicate = pkg: builtins.elem (nixpkgs.lib.getName pkg) [ ];
          };
          emacs-config = lib'.mkEmacsConfig { inherit pkgs; };
        in
        emacs-config.makeApps { lockDirName = "lock"; }
      );

      checks = forAllSystems (system: {
        pre-commit-check = inputs.git-hooks-nix.lib.${system}.run {
          src = ./.;
          hooks = {
            nixfmt-rfc-style = {
              enable = true;
              excludes = [ "lock/flake\\.nix" ];
            };
          };
        };
      });

      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          check = self.checks.${system}.pre-commit-check;
        in
        {
          default = pkgs.mkShell {
            inherit (check) shellHook;
            buildInputs = check.enabledPackages;
          };
        }
      );

      formatter = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          treefmtEval = inputs.treefmt-nix.lib.evalModule pkgs {
            projectRootFile = "flake.nix";
            programs = {
              nixfmt.enable = true;
              stylua.enable = true;
              taplo.enable = true;
              yamlfmt.enable = true;
            };
          };
        in
        treefmtEval.config.build.wrapper
      );
    };
}
