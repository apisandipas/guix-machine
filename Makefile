CONFIG_FILE = ./cablecar/reconfigure.scm
HOST=norrin
RDE_USER=bryan
GLP=./

home-build:
	HOST=$(HOST) GUILE_LOAD_PATH=$(GLP) RDE_USER=$(RDE_USER) RDE_TARGET=home \
	guix home build $(CONFIG_FILE)

home-reconfigure:
	HOST=$(HOST) GUILE_LOAD_PATH=$(GLP) RDE_USER=$(RDE_USER) RDE_TARGET=home \
	guix home reconfigure $(CONFIG_FILE)

system-build:
	HOST=$(HOST) GUILE_LOAD_PATH=$(GLP) RDE_USER=$(RDE_USER) RDE_TARGET=system \
	guix system build $(CONFIG_FILE)

system-reconfigure:
	HOST=$(HOST) GUILE_LOAD_PATH=$(GLP) RDE_USER=$(RDE_USER) RDE_TARGET=system \
	guix system reconfigure $(CONFIG_FILE)

system-init:
	HOST=$(HOST) GUILE_LOAD_PATH=$(GLP) RDE_USER=$(RDE_USER) RDE_TARGET=system \
	guix system init $(CONFIG_FILE) /mnt --substitute-urls="https://bordeaux.guix.gnu.org"

system-reconfigure:
	HOST=$(HOST) GUILE_LOAD_PATH=$(GLP) RDE_USER=$(RDE_USER) RDE_TARGET=system \
	guix system reconfigure $(CONFIG_FILE)

home-test:
	HOST=$(HOST) GUILE_LOAD_PATH=$(GLP) RDE_USER=$(RDE_USER) RDE_TARGET=home\
	guix home reconfigure ./cablecar/testing.scm
