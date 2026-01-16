# Copyright (C) 2025 Ohma Togaki
# SPDX-License-Identifier: MIT

{
  inputs,
  lib,
  pkgs,
  ...
}:
let
  inherit (builtins)
    readFile
    toFile
    ;
  inherit (lib)
    composeExtensions
    pipe
    ;

  mkInitFile =
    {
      initPath ? (../. + "/init.org"),
    }:
    pipe initPath [
      readFile
      (inputs.org-babel.tangleOrgBabel { })
      (toFile "init.el")
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
        initParser = inputs.twist.parseUsePackages {
          inherit (inputs.nixpkgs) lib;
        } { };
        inputOverrides = (import ../twist/inputs.nix) // {
          brk = _: _: {
            src = inputs.nix-filter.lib {
              root = inputs.self;
              include = [ "elisp" ];
            };
          };
        };
        localPackages = [
          # Exclude these packages from the lock file
          "brk"
        ];
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
    (inputs.twist.makeEnv (
      twistArgs
      // {
        inherit pkgs emacsPackage;
      }
    )).overrideScope
      (
        composeExtensions inputs.twist-overrides.overlays.twistScope (
          _final: prev: {
            elispPackages = prev.elispPackages.overrideScope (import ../twist/overrides.nix { inherit pkgs; });
          }
        )
      );
}
