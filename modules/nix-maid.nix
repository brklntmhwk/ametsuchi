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
  inherit (builtins) attrValues concatStringsSep listToAttrs;
  inherit (lib)
    getExe
    mkEnableOption
    mkIf
    mkMerge
    mkOption
    optional
    types
    ;
  inherit (pkgs) makeDesktopItem runCommandLocal;
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

  wrappedEmacs =
    runCommandLocal cfg.name
      {
        propagatedBuildInputs = [
          emacsConfig
        ];
      }
      ''
        mkdir -p $out/bin

        # Use "exec" instead of "makeWrapper" to allow safe runtime expansion
        # of variables; it wraps arguments in single quotes, which prevents
        # the shell from expanding runtime variables.
        cat > $out/bin/${cfg.name} <<EOF
        #!/bin/sh
        exec ${emacsConfig}/bin/emacs \
        --init-directory="\$HOME/${cfg.directory}" "\$@"
        EOF

        chmod +x $out/bin/${cfg.name}

        ${lib.optionalString cfg.emacsclient.enable "ln -t $out/bin -s ${emacsConfig.emacs}/bin/emacsclient"}
      '';

  fonts = attrValues {
    # Add font packages that will be used in your Emacs config.
    inherit (pkgs)
      moralerspace-hwnf
      sarasa-gothic
      noto-fonts-emoji
      symbola
      ;
    inherit (pkgs.nerd-fonts)
      symbols-only
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
      "TextEditor"
      "Development"
    ];
  };

  # https://github.com/viperML/nix-maid/commit/4ea39e76cdc8f8946bf4474a55962b2dfd8258fb
  userSubmodule = {
    config = mkIf cfg.enable {
      # Install packages in `users.users.${username}.packages` without
      # having to declare an option like `username`.
      packages = [
        fonts
        wrappedEmacs
      ]
      ++ optional cfg.icons.enable emacsConfig.icons
      ++ optional (!pkgs.stdenv.isDarwin) desktopItem;

      maid = {
        file = {
          home = listToAttrs [
            {
              name = "${cfg.directory}/init.el";
              value = {
                source = "${initFile}/init.el";
              };
            }
            {
              name = "${cfg.directory}/templates";
              value = {
                source = ../templates;
              };
            }
            {
              name = "${cfg.directory}/early-init.el";
              value = {
                source = ../early-init.el;
              };
            }
          ];
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
        description = "Name of the wrapper script.";
        default = "emacs";
        example = "my-emacs";
      };
      directory = mkOption {
        type = types.str;
        description = "Relative path in string to user-emacs-directory from the home directory";
        default = ".config/emacs";
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
          description = "Whether to install Emacs icons.";
          default = true;
        };
      };
      desktopItem = {
        desktopName = mkOption {
          type = types.str;
          description = "Long name of the desktop item.";
          default = "Emacs";
        };
        mimeTypes = mkOption {
          type = types.listOf types.str;
          description = "List of mime types associated with the wrapper.";
          default = [
            "text/plain"
            "inode/directory"
          ];
        };
      };
      extraFeatures = mkOption {
        type = types.listOf types.str;
        description = "Add extra features";
        default = [ ];
      };
      packageWrapped = mkOption {
        type = types.package;
        description = "The wrapped Emacs package.";
        readOnly = true;
        default = wrappedEmacs;
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf cfg.serviceIntegration.enable {
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
            ${pkgs.runtimeShell} -c 'source ${config.system.build.setEnvironment}; ${getExe cfg.packageWrapped} --fg-daemon
          '';
          Restart = "on-failure";
          # Emacs exits with exit code 15 (SIGTERM), when stopped by systemd.
          SuccessExitStatus = 15;
        };
        wantedBy = [ "graphical-session.target" ];
      };
    })
  ]);
}
