# Copyright (C) 2025 Ohma Togaki
# SPDX-License-Identifier: MIT

{ pkgs }:
_final: prev:
builtins.intersectAttrs prev {
  emacsql-sqlite = prev.emacsql-sqlite.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [ pkgs.sqlite ];

    postBuild = ''
      cd sqlite
      make
      cd ..
    '';
  });

  jinx = prev.jinx.overrideAttrs (
    old:
    let
      modSuffix = pkgs.stdenv.targetPlatform.extensions.sharedLibrary;
    in
    {
      nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.pkg-config ];
      buildInputs = (old.buildInputs or [ ]) ++ [ pkgs.enchant2 ];
      preBuild = ''
        NIX_CFLAGS_COMPILE="$($PKG_CONFIG --cflags enchant-2) $NIX_CFLAGS_COMPILE"
        $CC -I. -O2 -fPIC -shared -o jinx-mod${modSuffix} jinx-mod.c -lenchant-2
      '';
    }
  );

  # https://github.com/akirak/emacs-config/blob/1a76845c6f3740578efdc978da6db9b0ebe6bccc/emacs/overrides.nix
  brk = prev.brk.overrideAttrs (old: {
    # The libraries are improperly packaged, so disable byte-compilation for now.
    dontByteCompile = true;
  });
}
