# Copyright (C) 2025 Ohma Togaki
# SPDX-License-Identifier: MIT

# Based on:
# https://github.com/emacs-twist/twist.nix/commit/b566be881ee812b646f4201d18d1424d0f5127c6

# Take mkEmacsConfig as an arg to allow additional configs based on user customization.
mkEmacsConfig:
{
  config,
  options,
  lib,
  pkgs,
  ...
}:

let
  inherit (builtins) attrValues concatStringsSep;
  inherit (lib)
    getExe
    getExe'
    mkEnableOption
    mkIf
    mkOption
    optional
    optionalString
    types
    ;
  inherit (pkgs) makeDesktopItem runCommandLocal stdenv;
  inherit (stdenv.hostPlatform) isDarwin;
  cfg = config.programs.ametsuchi;

  emacsConfig = mkEmacsConfig {
    inherit pkgs;
    features = cfg.extraFeatures;
  };

  initFile = runCommandLocal "init.el" { } ''
    mkdir -p $out
    touch $out/init.el
    for file in ${concatStringsSep " " emacsConfig.initFiles}
    do
      cat "$file" >> $out/init.el
      echo >> $out/init.el
    done
  '';

  emacsBin = getExe emacsConfig;
  emacsclient = getExe' emacsConfig.emacs "emacsclient";

  wrappedEmacs =
    runCommandLocal cfg.name
      {
        propagatedBuildInputs = [
          emacsConfig
        ];
        # Suppress the build time warning, 'evaluation warning: getExe: Package emacs
        # does not have the meta.mainProgram attribute...'
        meta.mainProgram = "emacs";
      }
      ''
        mkdir -p $out/bin

        # Use "exec" instead of "makeWrapper" to allow safe runtime expansion
        # of variables; it wraps arguments in single quotes, which prevents
        # the shell from expanding runtime variables.
        cat > $out/bin/${cfg.name} <<EOF
        #!/bin/sh
        exec ${emacsBin} \
        --init-directory="\$HOME/${cfg.directory}" "\$@"
        EOF

        chmod +x $out/bin/${cfg.name}

        ${optionalString cfg.emacsclient.enable "ln -t $out/bin -s ${emacsclient}"}
      '';

  fonts = attrValues {
    # Add font packages that will be used in your Emacs config.
    inherit (pkgs)
      moralerspace-hw
      sarasa-gothic
      noto-fonts-color-emoji
      symbola
      ;
    inherit (pkgs.nerd-fonts)
      symbols-only
      ;
  };
  miscPkgs = attrValues {
    inherit (pkgs)
      emacs-lsp-booster # eglot-booster uses this.
      ;
  };

  desktopItem = makeDesktopItem {
    inherit (cfg) name;
    inherit (cfg.desktopItem) desktopName mimeTypes;
    comment = "Edit text";
    genericName = "Text Editor";
    exec = "${cfg.name} %F";
    icon = "emacs";
    startupNotify = true;
    startupWMClass = "Emacs";
    categories = [
      "Development"
      "TextEditor"
    ];
  };

  # https://github.com/viperML/nix-maid/commit/4ea39e76cdc8f8946bf4474a55962b2dfd8258fb
  userSubmodule =
    { config, ... }:
    {
      # Prevent it from trying to configure system users like 'chrony'.
      config = mkIf (cfg.enable && config.isNormalUser) {
        # Install packages in `users.users.${username}.packages` without
        # having to declare an option like `username`.
        packages = [
          wrappedEmacs
        ]
        ++ fonts
        ++ miscPkgs
        ++ optional cfg.icons.enable emacsConfig.icons
        ++ optional (!isDarwin) desktopItem;

        maid = {
          file = {
            home = {
              "${cfg.directory}/init.el".source = "${initFile}/init.el";
              "${cfg.directory}/templates".source = ../templates;
              "${cfg.directory}/early-init.el".source = ../early-init.el;
            };
          };
        };
      };
    };
in
{
  options = {
    users.users = mkOption {
      type = types.attrsOf (types.submodule userSubmodule);
    };

    programs.ametsuchi = {
      enable = mkEnableOption "Ametsuchi";
      name = mkOption {
        type = types.str;
        default = "emacs";
        description = "Name of the wrapper script.";
        example = "my-emacs";
      };
      directory = mkOption {
        type = types.str;
        default = ".config/emacs";
        description = ''
          Relative path in string to user-emacs-directory from the home directory.
        '';
        example = ".local/share/emacs";
      };
      emacsclient = {
        enable = mkEnableOption "emacsclient";
      };
      serviceIntegration = {
        enable = mkEnableOption (
          lib.mdDoc ''
            Enable service integration. For now, only systemd is supported.
          ''
        );
      };
      icons = {
        enable = mkOption {
          type = types.bool;
          default = true;
          description = "Whether to install Emacs icons.";
          example = false;
        };
      };
      desktopItem = {
        desktopName = mkOption {
          type = types.str;
          default = "Emacs";
          description = "Long name of the desktop item.";
          example = "My Emacs";
        };
        mimeTypes = mkOption {
          type = types.listOf types.str;
          default = [
            "text/plain"
            "inode/directory"
          ];
          description = "List of mime types associated with the wrapper.";
          example = [ "text/plain" ];
        };
      };
      extraFeatures = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Extra features to add to.";
        example = literalExpression ''
          [
          
          ]
        '';
      };
      packageWrapped = mkOption {
        type = types.package;
        default = wrappedEmacs;
        readOnly = true;
        description = "The wrapped Emacs package.";
      };
    };
  };

  config = mkIf cfg.enable (
    mkIf cfg.serviceIntegration.enable {
      # Based on:
      # https://github.com/NixOS/nixpkgs/commit/958ae22cc3d4dcf6c9ef008ec2c582b4fe9fa083
      systemd.user.services.emacs = {
        unitConfig = {
          After = [ "graphical-session.target" ];
          Description = "Emacs: Extensible and self-documenting text editor";
          PartOf = [ "graphical-session.target" ];
          # Do not kill the Emacs session; it may contain unsaved work.
          # https://github.com/nix-community/home-manager/commit/bca7415de4565c25a1843cc7baed5b783d70240f
          X-RestartIfChanged = false;
        };
        serviceConfig = {
          Type = "notify";
          ExecStart = ''
            ${pkgs.runtimeShell} -l -c "${getExe cfg.packageWrapped} --fg-daemon"
          '';
          Restart = "on-failure";
          # Emacs exits with exit code 15 (SIGTERM), when stopped by systemd.
          SuccessExitStatus = 15;
        };
        wantedBy = [ "graphical-session.target" ];
      };
    }
  );
}
