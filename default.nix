{ pkgs ? import <nixpkgs> {} }:

# https://freux.fr/posts/env.html

let
  hp = pkgs.haskellPackages;  # change this to use a different compiler
  c2n = name: fp: attrs: hp.callCabal2nix name fp attrs;
  fsrc = src: pkgs.nix-gitignore.gitignoreSource [] src;
in
  rec {
    dbus = pkgs.haskell.lib.appendPatch hp.dbus ./dbus.patch;
    xmonad = c2n "xmonad" (fsrc ./vendor/xmonad) {};
    xmonad-contrib = c2n "xmonad-contrib" (fsrc ./vendor/xmonad-contrib) { inherit xmonad; };
    xmonad-samdoshi = c2n "xmonad-samdoshi" (fsrc ./.) { inherit dbus xmonad xmonad-contrib; };
    env = hp.shellFor {
      name = "xmonad-samdoshi-env";
      withHoogle = true;
      packages = p: [ xmonad-samdoshi ];
      buildInputs = with pkgs; [
        hp.apply-refact
        hp.cabal-install
        hp.hasktags
        hp.hlint
        hp.stylish-haskell
      ];
    };
  }
