# Copyright (C) 2025 Ohma Togaki
# SPDX-License-Identifier: MIT

{
	dired-collapse = _: prev: {
		packageRequires = {
			dash = "0";
			f = "0";
			dired-hack-utils = "0";
		} // prev.packageRequires;
	};

	dired-filter = _: prev: {
		packageRequires = {
			dired-hack-utils = "0";
			f = "0";
		} // prev.packageRequires;
	};
}
