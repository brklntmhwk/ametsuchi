# Copyright (C) 2025 Ohma Togaki
# SPDX-License-Identifier: MIT

# Based on:
# https://github.com/emacs-twist/twist.nix/commit/b566be881ee812b646f4201d18d1424d0f5127c6
{
  config,
  options,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mkMerge
    mkOption
    mkOptionType
    optionalString
    types
    ;
  cfg = config.programs.emacs-twist;
  emacsConfig = cfg.config;

  hasYakumoUser = options ? yakumo.user && config.yakumo.user ? name;
  username = if hasYakumoUser then config.yakumo.user.name else null;

  initFile = pkgs.runCommandLocal "init.el" { } ''
    mkdir -p $out
    touch $out/init.el
    for file in ${builtins.concatStringsSep " " emacsConfig.initFiles}
    do
      cat "$file" >> $out/init.el
      echo >> $out/init.el
    done
  '';

  wrapper =
    pkgs.runCommandLocal cfg.name
      {
        propagatedBuildInputs = [
          emacsConfig
        ];
        nativeBuildInputs = [
          pkgs.makeWrapper
        ];
        meta.mainProgram = cfg.name;
      }
      ''
        mkdir -p $out/bin

        # Create the wrapper script
        cat > $out/bin/${cfg.name} <<EOF
        #!/bin/sh
        exec ${emacsConfig}/bin/emacs \\
          --init-directory "\$HOME/${cfg.directory}" \\
          ${optionalString (cfg.earlyInitFile != null) "--early-init-file ${cfg.earlyInitFile} \\"}
          ${optionalString cfg.createInitFile "-q -l ${initFile}/init.el \\"}
          "\$@"
        EOF

        chmod +x $out/bin/${cfg.name}

        ${optionalString cfg.emacsclient.enable "ln -t $out/bin -s ${emacsConfig.emacs}/bin/emacsclient"}
      '';

  desktopItem = pkgs.makeDesktopItem {
    inherit (cfg) name;
    inherit (cfg.desktopItem) desktopName mimeTypes;
    comment = "Edit text";
    genericName = "Text Editor";
    exec = "${cfg.name} %F";
    icon = "emacs";
    startupNotify = true;
    startupWMClass = "Emacs";
    categories = [
      "TextEditor"
      "Development"
    ];
  };
in
{
  options.programs.emacs-twist = {
    enable = mkEnableOption "Emacs Twist";
    name = mkOption {
      type = types.str;
      description = "Name of the wrapper script";
      default = "emacs";
      example = "my-emacs";
    };
    directory = mkOption {
      type = types.str;
      description = "Relative path from the user's home directory to user-emacs-directory";
      default = ".config/emacs";
      example = ".local/share/emacs";
    };
    createInitFile = mkOption {
      type = types.bool;
      description = "Whether to load the generated init.el (disabling the user's own init file)";
      default = false;
    };
    earlyInitFile = mkOption {
      type = types.nullOr types.path;
      description = "Path to early-init.el. If provided, it is passed via --early-init-file.";
      default = null;
    };
    createManifestFile = mkOption {
      type = types.bool;
      description = "Whether to expose the manifest file. (Note: In NixOS module this does not symlink to home, but ensures internal variables are set)";
      default = false;
    };
    manifestFileName = mkOption {
      type = types.str;
      description = "Name of the manifest file (unused in NixOS module, kept for compatibility)";
      default = "twist-manifest.json";
    };
    config = mkOption {
      type = mkOptionType {
        name = "twist";
        description = "Configuration of emacs-twist";
        check = c: c ? initFiles && c ? emacs;
      };
    };
    wrapper = mkOption {
      type = types.package;
      description = "The wrapper derivation";
      readOnly = true;
      default = wrapper;
    };
    emacsclient = {
      enable = mkEnableOption "emacsclient";
    };
    serviceIntegration = {
      enable = mkEnableOption (
        lib.mdDoc ''
          Enable service integration. This configures the systemd user service provided by the package.
        ''
      );
    };
    icons = {
      enable = mkOption {
        type = types.bool;
        description = "Whether to install Emacs icons";
        default = true;
      };
    };
    desktopItem = {
      desktopName = mkOption {
        type = types.str;
        description = "Long name of the desktop item";
        default = "Emacs";
      };
      mimeTypes = mkOption {
        type = types.listOf types.str;
        description = "List of mime types associated with the wrapper";
        default = [
          "text/plain"
          "inode/directory"
        ];
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf hasYakumoUser {
      user.packages = [
        wrapper
      ]
      ++ lib.optional cfg.icons.enable emacsConfig.icons
      ++ lib.optional (!pkgs.stdenv.isDarwin) desktopItem;
    })
    (mkIf (!hasYakumoUser) {
      environment.systemPackages = [
        wrapper
      ]
      ++ lib.optional cfg.icons.enable emacsConfig.icons
      ++ lib.optional (!pkgs.stdenv.isDarwin) desktopItem;
    })
    {
      # Configure the systemd user service.
      services.emacs = lib.mkIf cfg.serviceIntegration.enable {
        enable = true;
        package = wrapper;
      };
    }
  ]);
}
