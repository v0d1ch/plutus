{ inputs, cell }@block:

rec {
  plutus-shell = import ./plutus-shell.nix block;

  # TODO(std)
  # profiled-plutus-shell = import ./profiled-plutus-shell.nix block;

  default = plutus-shell;
}
