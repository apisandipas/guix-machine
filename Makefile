CONFIG_FILE = ./cablecar/reconfigure.scm
HOSTS = norrin
RDE_USER=bryan
GLP = ./

home-build:
	GUILE_LOAD_PATH=$(GLP) RDE_USER=$(RDE_USER) RDE_TARGET=home \
	guix home build $(CONFIG_FILE)

home-reconfigure:
	GUILE_LOAD_PATH=$(GLP) RDE_USER=$(RDE_USER) RDE_TARGET=home \
	guix home reconfigure $(CONFIG_FILE)

system-build:
	GUILE_LOAD_PATH=$(GLP) RDE_USER=$(RDE_USER) RDE_TARGET=system \
	guix system build $(CONFIG_FILE)

system-reconfigure:
	GUILE_LOAD_PATH=$(GLP) RDE_USER=$(RDE_USER) RDE_TARGET=system \
	guix system reconfigure $(CONFIG_FILE)

system-init:
	GUILE_LOAD_PATH=$(GLP) RDE_USER=bryan RDE_TARGET=system \
	guix system init $(CONFIG_FILE) /mnt
