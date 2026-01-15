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
    concatStringsSep
    readFile
    toFile
    ;
  inherit (lib)
    composeExtensions
    escapeShellArg
    escapeShellArgs
    getName
    makeBinPath
    mapAttrsToList
    pipe
    ;
  inherit (pkgs)
    makeWrapper
    symlinkJoin
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

  # TODO: Make this an inch-perfect match with that in Yakumo.
  mkAppWrapper =
    {
      pkg,
      name ? null,
      bin ? null,
      flags ? [ ],
      env ? { },
      deps ? [ ],
      desktop ? null,
    }:
    let
      binName =
        if bin != null then
          bin
        else if (pkg.meta ? mainProgram) then
          pkg.meta.mainProgram
        else
          (lib.getName pkg);
      finalName = if name != null then name else "${getName pkg}-wrapped";
      flagsStr = escapeShellArgs flags;
      envStr = concatStringsSep " " (mapAttrsToList (k: v: "--set ${k} ${escapeShellArg v}") env);
      pathStr = if deps == [ ] then "" else "--prefix PATH : ${makeBinPath deps}";
      desktopItem =
        if desktop == null then
          null
        else
          let
            # Mandatory Field Check
            _ =
              if !(desktop ? desktopName) then
                throw "'desktop.desktopName' is required when 'desktop' is set."
              else
                null;
          in
          {
            name = desktop.name or finalName;
            type = desktop.type or "Application";
            desktopName = desktop.desktopName;
            genericName = desktop.genericName or null;
            noDisplay = desktop.noDisplay or null;
            comment = desktop.comment or null;
            icon = desktop.icon or null;
            onlyShowIn = desktop.onlyShowIn or [ ];
            notShowIn = desktop.notShowIn or [ ];
            dbusActivatable = desktop.dbusActivatable or null;
            terminal = desktop.terminal or false;
            mimeTypes = if (desktop ? mimeTypes) then concatStringsSep ";" desktop.mimeTypes else null;
            categories = if (desktop ? categories) then concatStringsSep ";" desktop.categories else null;
            keywords = if (desktop ? keywords) then concatStringsSep ";" desktop.keywords else null;
            startupNotify = desktop.startupNotify or false;
            args = desktop.args or "";
          };
    in
    symlinkJoin {
      name = finalName;
      paths = [ pkg ];
      buildInputs = [ makeWrapper ];
      desktopTerminal = if (desktopItem != null && desktopItem.terminal) then "true" else "false";
      desktopStartupNotify =
        if (desktopItem != null && desktopItem.startupNotify) then "true" else "false";
      postBuild = ''
        if [ ! -x "$out/bin/${binName}" ]; then
            echo "Error: Binary '${binName}' not found in package ${getName pkg}!"
            echo "       Please assume the auto-detection failed."
            echo "       Pass the 'bin' argument explicitly to mylib.wrap."
            echo "       Available binaries: $(ls $out/bin)"
            exit 1
        fi

        wrapProgram $out/bin/${binName} \
          ${envStr} \
          --add-flags "${flagsStr}" \
          ${pathStr}

        ${optionalString (desktop != null) ''
          mkdir -p $out/share/applications
          cat > $out/share/applications/${desktopItem.name}.desktop <<EOF
          [Desktop Entry]
          Type=Application
          Name=${desktopItem.desktopName}
          Exec=$out/bin/${binName} ${desktopItem.args}
          Terminal=$desktopTerminal
          StartupNotify=$desktopStartupNotify
          ${optionalString (desktopItem.genericName != null) "GenericName=${desktopItem.genericName}"}
          ${optionalString (desktopItem.comment != null) "Comment=${desktopItem.comment}"}
          ${optionalString (desktopItem.icon != null) "Icon=${desktopItem.icon}"}
          ${optionalString (desktopItem.categories != null) "Categories=${desktopItem.categories};"}
          ${optionalString (desktopItem.mimeTypes != null) "MimeType=${desktopItem.mimeTypes};"}
          ${optionalString (desktopItem.keywords != null) "Keywords=${desktopItem.keywords};"}
          EOF
        ''}
      '';
    };
}
