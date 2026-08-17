# Copyright (C) 2025 Ohma Togaki
# SPDX-License-Identifier: MIT

{
  inputs,
  lib,
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
  inherit (pkgs) callPackage;
  inherit (inputs)
    emacs-overlay
    nixpkgs
    nix-filter
    org-babel
    self
    twist
    twist-overrides
    ;

  mkInitFile =
    {
      initPath ? (../. + "/init.org"),
    }:
    pipe initPath [
      readFile
      (org-babel.lib.tangleOrgBabel { })
      (toFile "init.el")
    ];
in
{
  inherit mkInitFile;

  mkEmacsConfig =
    {
      pkgs,
      emacsPackage ? emacs-overlay.packages.${pkgs.system}.emacs-git-pgtk,
      initFile ? mkInitFile { },
      features ? [ ],
      prependToInitFile ? null,
    }:
    let
      twistArgs = {
        # https://github.com/akirak/emacs-config/commit/9940dc91e3ecf2b3faf861c2492867c9165202f3
        extraSiteStartElisp = ''
          (add-to-list 'treesit-extra-load-path "${
            callPackage ./treesit-grammars.nix { inherit inputs; }
          }/lib/")
        '';
        exportManifest = true; # Required to use hot-reloading twist.el offers
        initFiles = [ initFile ];
        initParser = twist.lib.parseUsePackages {
          inherit (nixpkgs) lib;
        } { };
        inputOverrides = (import ../twist/inputs.nix) // {
          brk = _: _: {
            src = nix-filter.lib {
              root = self;
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
    (twist.lib.makeEnv (
      twistArgs
      // {
        inherit pkgs emacsPackage;
      }
    )).overrideScope
      (
        composeExtensions twist-overrides.overlays.twistScope (
          _final: prev: {
            elispPackages = prev.elispPackages.overrideScope (import ../twist/overrides.nix { inherit pkgs; });
          }
        )
      );
}
