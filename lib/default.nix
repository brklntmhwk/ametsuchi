# Copyright (C) 2025 Ohma Togaki
# SPDX-License-Identifier: MIT

{ inputs, lib, ... }:
let
  mkInitFile =
    {
      initPath ? (../. + "/init.org"),
    }:
    lib.pipe initPath [
      builtins.readFile
      (inputs.org-babel.lib.tangleOrgBabel { })
      (builtins.toFile "init.el")
    ];
in
{
  inherit mkInitFile;

  mkEmacsConfig =
    {
      pkgs,
      emacsPackage ? inputs.emacs-overlay.packages.${pkgs.system}.emacs-git-pgtk,
      initFile ? mkInitFile { },
      features ? [ ],
      prependToInitFile ? null,
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
        registries = [
          {
            type = "melpa";
            path = ../recipes;
          }
        ]
        ++ (import ../twist/registries.nix { inherit inputs; });
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
  # TODO: implement a wrapper lib function for making tentative emacs env
  # NOTE: Consider refactoring this and move to another file (e.g., pkgs.nix)
  # mkTmpEmacsEnvWrapper = { ... }: { };
}
