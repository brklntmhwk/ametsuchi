# Copyright (C) 2025 Ohma Togaki
# SPDX-License-Identifier: MIT

# https://github.com/akirak/emacs-config/commit/14ba1e6673c8c179f122b8bec2397b88dc74b04f

# Take mkEmacsConfig as an arg to make additional configs based on user customization.
mkEmacsConfig:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkDefault
    mkIf
    mkOption
    types
    ;
  cfg = config.programs.emacs-twist;
in
{
  options = {
    programs.emacs-twist = {
      settings = {
        extraFeatures = mkOption {
          type = types.listOf types.str;
          description = "Add extra features";
          default = [ ];
        };
      };
    };
  };

  config = mkIf cfg.enable {
    programs.emacs-twist = {
      emacsclient.enable = true;
      serviceIntegration.enable = mkDefault true;
      createInitFile = true;
      createManifestFile = true;
      directory = ".local/share/emacs"; # Consider it more of a datum than a config.
      earlyInitFile = ../early-init.el;
      config = mkEmacsConfig {
        inherit pkgs;
        features = cfg.settings.extraFeatures;
      };
    };

    home.packages = builtins.attrValues {
      # Add font packages that will be used in your Emacs config.
      inherit (pkgs)
        moralerspace-hwnf
        sarasa-gothic
        noto-fonts-emoji
        symbola
        ;
    };

    # Generate a desktop file for emacsclient.
    services.emacs.client.enable = cfg.serviceIntegration.enable;

    # Deploy data files.
    xdg.dataFile = {
      "emacs/templates" = {
        source = ../templates;
        recursive = true;
      };
    };
  };
}
