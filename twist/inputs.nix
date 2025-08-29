# Copyright (C) 2025 Ohma Togaki
# SPDX-License-Identifier: MIT

{
  # https://github.com/akirak/emacs-config/commit/875023f87fb4da96c766818698a6577f31566d6c
  org = _: _: {
    origin = {
      # git.savannah.org is unstable
      type = "github";
      owner = "elpa-mirrors";
      repo = "org-mode";
      ref = "bugfix";
    };
  };

  # https://github.com/akirak/emacs-config/commit/58f068ad8e4ade7e9c5201bfb1026e9e3a409f6d
  dired-collapse = _: prev: {
    packageRequires = {
      dash = "0";
      f = "0";
      dired-hacks-utils = "0";
    }
    // prev.packageRequires;
  };

  dired-filter = _: prev: {
    packageRequires = {
      dired-hacks-utils = "0";
      f = "0";
    }
    // prev.packageRequires;
  };

  # https://github.com/akirak/emacs-config/commit/f34db5f4ed71e79788c879973840fd549ffe65a2
  persist = _: prev: {
    files = builtins.removeAttrs prev.files [ "persist.texi" ];
  };
}
