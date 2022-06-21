CONFIG_FILE = ./cablecar/reconfigure.scm
GLP = ../rde

home-build:
	GUILE_LOAD_PATH=$(GLP) RDE_TARGET=home \
	guix home build $(CONFIG_FILE)

home-reconfigure:
	GUILE_LOAD_PATH=$(GLP) RDE_TARGET=home \
	guix home reconfigure $(CONFIG_FILE)

system-build:
	GUILE_LOAD_PATH=$(GLP) RDE_TARGET=system \
	guix system build $(CONFIG_FILE)

system-reconfigure:
	GUILE_LOAD_PATH=$(GLP) RDE_TARGET=system \
	guix system reconfigure $(CONFIG_FILE)

.PHONY: help
