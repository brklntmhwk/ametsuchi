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
            pkgs.callPackage ./treesit-grammars.nix { inherit inputs; }
          }/lib/")

          ;; Suppress the "Missing ‘lexical-binding’ cookie in..." warning at startup.
          ;; This prevents legacy upstream files (e.g., mozc.el) from triggering missing
          ;; lexical-binding warnings introduced in Emacs 30+.
          (defun brk/suppress-lexical-binding-warning-ad (orig-fn type message &rest args)
            (if (and (eq type 'files)
                     (stringp message)
                     ;; Periods surrounding "lexical-binding" handle the difference in
                     ;; quotation marks (e.g., ` ' and ' ') used.
                     (string-match-p "Missing .lexical-binding. cookie" message))
                nil
              (apply orig-fn type message args)))
          (advice-add 'display-warning :around #'brk/suppress-lexical-binding-warning-ad)
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
