# Copyright (C) 2025 Ohma Togaki
# SPDX-License-Identifier: MIT

{ inputs, ... }:
{
	imports = [ inputs.git-hooks-nix.flakeModules ];

	perSystem = { pkgs, system, config, ... }:
	{
		pre-commit = {
			check.enable = true;
			settings.hooks = {
				nixfmt-rfc-style = {
					enable = true;
					excludes = [ "lock/flake\\.nix" ];
				};
			};
		};

		devShells = {
			default = config.pre-commit.devShell;
		};
	};
}
