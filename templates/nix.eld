nix-mode

;; https://github.com/terlar/emacs-config/commit/f898f84c0f6222421e5dba4bca7c30c83610c90e
(sha256-sri-hash "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=")
(mkOption > "mkOption {" n> "type = " (p "type" type) ";" n> "default = " (p "default" default) ";" n> "description = \"" (p "description" description) "\";" n> "};")
(fetchFromGitHub > "pkgs.fetchFromGitHub {" n> "owner = \"" (p "owner" owner) "\";" n> "repo = \"" (p "repo" repo) "\";" n> "rev = \"" (p "rev" rev) "\";" n> "hash = \"" (p "hash" hash) "\";" n> "};")

(fetchgit > "pkgs.fetchgit {" n> "url = \"" (p "url" url) "\";" n> "rev = \"" (p "rev" rev) "\";" n> "hash = \"" (p "hash" hash) "\";" n> "fetchSubmodules = \"" (p "fetchSubmodules (true/false)" fetchSubmodules) "\";" n> "};")
(fetchFromSourcehut > "pkgs.fetchFromSourcehut {" n> "owner = \"~" (p "owner" owner) "\";" n> "repo = \"" (p "repo" repo) "\";" n> "rev = \"" (p "rev" rev) "\";" n> "hash = \"" (p "hash" hash) "\";" n> "};")
