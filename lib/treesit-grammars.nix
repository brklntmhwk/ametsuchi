{
  inputs,
  emacs,
  tree-sitter,
}:

let
  inherit (emacs.pkgs) treesit-grammars;
in
treesit-grammars.with-grammars (
  _:
  tree-sitter.allGrammars
  ++ [
    (tree-sitter.buildGrammar {
      language = "astro";
      version = "0";
      src = inputs.tree-sitter-astro.outPath;
    })
  ]
)
