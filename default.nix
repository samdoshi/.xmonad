{ pkgs ? import <nixpkgs> {} }:

# https://freux.fr/posts/env.html

let
  pinned = import (builtins.fetchTarball {
    name = "nixos-pinned-xmonad";
    url = "https://github.com/nixos/nixpkgs/archive/f0fec244ca380b9d3e617ee7b419c59758c8b0f1.tar.gz";
    sha256 = "0ga51457fb30b8j9v8is7wwf9ld9p51nizm8yhj09l23qpyh8np9";
  }) {};
  # pinned = pkgs;
  hp = pinned.haskellPackages;  # change this to use a different compiler
  c2n = name: fp: attrs: hp.callCabal2nix name fp attrs;
  fsrc = src: pinned.nix-gitignore.gitignoreSource [] src;
in
  rec {
    dbus = pinned.haskell.lib.appendPatch hp.dbus ./dbus.patch;
    xmonad = c2n "xmonad" (fsrc ./vendor/xmonad) {};
    xmonad-contrib = c2n "xmonad-contrib" (fsrc ./vendor/xmonad-contrib) { inherit xmonad; };
    xmonad-samdoshi = c2n "xmonad-samdoshi" (fsrc ./.) { inherit dbus xmonad xmonad-contrib; };
    env = hp.shellFor {
      name = "xmonad-samdoshi-env";
      withHoogle = true;
      packages = p: [ xmonad-samdoshi ];
      nativeBuildInputs = with pinned; [
        cacert
      ];
      buildInputs = with pinned; [
        hp.apply-refact
        hp.cabal-install
        hp.hasktags
        hp.hlint
        hp.stylish-haskell
      ];
    };
  }
