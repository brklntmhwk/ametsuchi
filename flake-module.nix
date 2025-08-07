# Copyright (C) 2025 Ohma Togaki
# SPDX-License-Identifier: MIT

{ inputs, lib, nixpkgs, ... }:
let
	lib' = import ./lib {
		inherit inputs;
		inherit (nixpkgs) lib;
	};

	ob = inputs.org-babel.lib;
	initPath = "./init.org";
	initFile = lib.pipe initPath [
		builtins.readFile
		(ob.tangleOrgBabel { })
		(builtins.toFile "init.el")
	];
in
{
	flake = {
		homeModules.twist = {
			imports = [
				inputs.twist.homeModules.emacs-twist
				(import ./modules/home-manager.nix lib'.mkEmacsConfig)
			];
		};
	};

	perSystem =
		{
			config,
			emacsConfig,
			pkgs,
			system,
			...
		}: {
			_module.args.pkgs = import inputs.nixpkgs {
				inherit system;
				# https://nixos.wiki/wiki/Unfree_Software
				config.allowUnfreePredicate =
					pkg: builtins.elem (lib.getName pkg) [
						# Add unfree package names that should be allowed to install here.
					];
			};
			_module.args.emacsConfig = lib'.mkEmacsConfig {
				inherit pkgs initFile;
			};

			packages = {
				inherit emacsConfig;

				emacsclient =
					inputs.legacyPackages.${system}.runCommandLocal "emacsclient"
						{ propagateBuildInputs = [ emacsConfig.emacs ]; }
						''
							mkdir -p $out/bin
ln -t $out/bin -s ${emacsConfig.emacs}/bin/emacsclient
						'';
			};

			apps = emacsConfig.makeApps { lockDirName = "lock"; };
		};
}
