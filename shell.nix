let
  # Fix the current version of the universe we are working with
  pkgs = import (fetchTarball {
    #     https://github.com/NixOS/nixpkgs/commit/6702acaf561f7a1326d5745031549bb7147f1882
    url = https://github.com/NixOS/nixpkgs/archive/6702acaf561f7a1326d5745031549bb7147f1882.tar.gz;
  }) {};

  # Alias pkg versions
  erlang = pkgs.erlangR21;

in

with pkgs;


mkShell {
  buildInputs = [
    # Base utilities
    zsh
    coreutils
    envsubst
    git

    # Languages
    erlang

    # Libraries
    openssl
  ];
}
