# Copyright (C) 2025 Ohma Togaki
# SPDX-License-Identifier: MIT

# https://github.com/akirak/emacs-config/commit/14ba1e6673c8c179f122b8bec2397b88dc74b04f

# Take mkEmacsConfig as an arg to allow additional configs based on user customization.
{
  inputs,
  mkEmacsConfig,
}:
{
  config,
  options,
  lib,
  pkgs,
  ...
}:

let
  inherit (builtins) attrValues removeAttrs;
  inherit (lib)
    evalModules
    mkEnableOption
    mkIf
    mkMerge
    mkOption
    types
    ;
  cfg = config.programs.ametsuchi;

  # Get the upstream (twist.nix) module options.
  twistMods = evalModules {
    modules = [
      inputs.twist.homeModules.emacs-twist
      {
        config._module.check = false;
      }
    ];
    specialArgs = {
      inherit config lib pkgs;
    };
  };
  twistOpts = twistMods.options.programs.emacs-twist;

  # Exclude these options since they will be overridden or their values
  # will be set on this Ametsuchi layer.
  excludedOptions = [
    "enable"
    "createInitFile"
    "createManifestFile"
    "manifestFileName"
    "earlyInitFile"
    "config"
  ];
  inheritedOptions = removeAttrs twistOpts excludedOptions;
in
{
  imports = [
    inputs.twist.homeModules.emacs-twist
  ];

  options.programs.ametsuchi = inheritedOptions // {
    enable = mkEnableOption "Ametsuchi";
    extraFeatures = mkOption {
      type = types.listOf types.str;
      description = "Add extra features";
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    programs.emacs-twist =
      let
        inherit (builtins) isAttrs;
        inherit (lib)
          getAttrFromPath
          isOption
          mapAttrs
          mkAliasDefinitions
          mkDefault
          ;
        mkRecursiveAlias =
          currentOpts: currentPath:
          mapAttrs (
            n: v:
            if isOption v then
              mkAliasDefinitions (getAttrFromPath (currentPath ++ [ n ]) options.programs.ametsuchi)
            else if isAttrs v then
              mkRecursiveAlias v (currentPath ++ [ n ])
            else
              { }
          ) currentOpts;
      in
      mkMerge [
        # e.g., `{ name = mkAliasDefinitions options.programs.ametsuchi.name; ... }`
        (mkRecursiveAlias inheritedOptions [ ])
        {
          enable = true;
          createInitFile = true;
          earlyInitFile = ../early-init.el;
          # Explicitly set a default value for this to prevent the "no value defined" error.
          # It looks like the upstream doesn't specify the default value.
          emacsclient.enable = mkDefault false;
          config = mkEmacsConfig {
            inherit pkgs;
            features = cfg.extraFeatures;
          };
        }
      ];

    home.packages = attrValues {
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
