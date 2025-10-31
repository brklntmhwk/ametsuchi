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
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
    nix-bwrapper.url = "github:Naxdy/nix-bwrapper";
    nix-filter.url = "github:numtide/nix-filter";

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
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      imports = [
        flake-parts.flakeModules.partitions
        ./flake-module.nix
      ];

      partitions = {
        dev = {
          extraInputsFlake = ./partitions/dev;
          module = {
            imports = [ ./partitions/dev/flake-module.nix ];
          };
        };
      };

      partitionedAttrs = {
        # checks = "dev";
        devShells = "dev";
        # packages = "dev";
      };
    };
}
