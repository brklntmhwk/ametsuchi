# Copyright (C) 2025 Ohma Togaki
# SPDX-License-Identifier: MIT

{
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
}
