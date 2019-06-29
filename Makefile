.PHONY: build clean clean-all install

build:
	nix build -f . xmonad-samdoshi

clean:
	rm -rf dist
	rm -rf dist-newstyle
	rm -rf .ghc.environment*
	rm -rf result

install:
	nix-env -f . -iA xmonad-samdoshi
