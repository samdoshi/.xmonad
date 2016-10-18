.PHONY: build install

build:
	stack build

install: build
	stack install --silent --local-bin-path $(HOME)/.local/bin xmonad-samdoshi:xmonad
