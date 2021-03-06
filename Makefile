.PHONY: build clean clean-all install

build-cabal:
	cabal new-build

build-nix:
	nix-build -A xmonad-samdoshi

clean:
	rm -rf dist
	rm -rf dist-newstyle
	rm -rf .ghc.environment*
	rm -rf result

install:
	nix-env -f . -iA xmonad-samdoshi
