.PHONY: build clean clean-all install

build:
	stack build

clean:
	stack clean xmonad-samdoshi

clean-all:
	stack clean

install: build
	stack install --silent --local-bin-path $(HOME)/.local/bin xmobar
	stack install --silent --local-bin-path $(HOME)/.local/bin xmonad-samdoshi:xmonad
