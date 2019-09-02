DEBUG ?=
REBAR  = DEBUG=$(DEBUG) ./3rdparty/beam/rebar3/rebar3

RELEASE_NAME = facts
BUILD_DIR = ./_build

PROFILE ?= local
CONFIG_FILES = $(shell find config -name "*.config.src")

COOKIE ?= local-cookie
SNAME  ?= $(shell whoami)@facts.local

################################################################################
#
# System Targets
#
################################################################################
all: shell

.PHONY: shell
shell:
	$(REBAR) shell \
		--setcookie $(COOKIE) \
		--sname $(SNAME) \
		--config config/$(PROFILE).config

.PHONY: deps
deps:
	$(REBAR) get-deps


################################################################################
#
# Standard Targets
#
################################################################################

.PHONY: clean
clean:
	$(REBAR) clean
	rm -rf $(BUILD_DIR)

# Empty target to force targets.
.PHONY: FORCE
FORCE:



################################################################################
#
# Config files.
#
#	Template over *.config.src files, generating *.config files by substituting
#	environment variables.
#
################################################################################

.PHONY: config
config: config/$(PROFILE).config
config/%.config: config/%.config.src FORCE
	envsubst < $< > $@
