# Copyright (C) 2025 Ohma Togaki
# SPDX-License-Identifier: MIT

{ inputs, lib, ... }:
{
  mkEmacsConfig =
    {
      pkgs,
      emacsPackage ? inputs.emacs-overlay.packages.${pkgs.system}.emacs-git-pgtk,
      initFile,
    }:
    let
      twistArgs = {
        # https://github.com/akirak/emacs-config/commit/9940dc91e3ecf2b3faf861c2492867c9165202f3
        extraSiteStartElisp = ''
          			(add-to-list 'treesit-extra-load-path "${
               pkgs.emacs.pkgs.treesit-grammars.with-grammars (
                 _:
                 (pkgs.tree-sitter.override {
                   extraGrammars = {
                     tree-sitter-astro = {
                       src = inputs.tree-sitter-astro.outPath;
                     };
                   };
                 }).allGrammars
               )
             }/lib/")
          		'';
        exportManifest = true; # Required to use hot-reloading twist.el offers
        initFiles = [ initFile ];
        initParser = inputs.twist.lib.parseUsePackages {
          inherit (inputs.nixpkgs) lib;
        } { };
        inputOverrides = import ../twist/inputs.nix;
        lockDir = ../lock;
        nativeCompileAheadDefault = true;
        registries = import ../twist/registries.nix { inherit inputs; };
      };
    in
    (inputs.twist.lib.makeEnv (
      twistArgs
      // {
        inherit pkgs emacsPackage;
      }
    )).overrideScope
      (
        lib.composeExtensions inputs.twist-overrides.overlays.twistScope (
          _final: prev: {
            elispPackages = prev.elispPackages.overrideScope (import ../twist/overrides.nix { inherit pkgs; });
          }
        )
      );
}
