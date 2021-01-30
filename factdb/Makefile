export

#################################################################################
# Common Postgres setup.
#################################################################################

BUILDSTAMP=$(shell date -Is | sed 's/://g; s/-//g; s/+//g')
GIT_COMMIT=$(shell git rev-parse --short HEAD)
VERSION=$(shell cat mix.exs| grep version | awk -F' ' '{ print $$2 }' | sed 's/[\"|,]//g')
IMAGE_NAME=registry.mainframe-0:32000/wittgenstein/factdb:$(VERSION)-$(GIT_COMMIT)-$(BUILDSTAMP)

#################################################################################
# Service Specific Env
#################################################################################

FACTDB_GRPC_PORT ?= 21001
FACTDB_HEALTHZ_HTTP_PORT ?= 21000
FACTDB_OPENTELEMETRY_HOST ?= opentelemetry.foundation.svc.cluster.local
FACTDB_OPENTELEMETRY_PORT ?= 9411

#################################################################################
# Targets
#################################################################################

.PHONY: setup
setup:
	mix deps.get
	mix escript.install hex protobuf

.PHONY: codegen
codegen:
	protoc -I priv/protos --elixir_out=plugins=grpc:./lib/ priv/protos/*.proto

.PHONY: fmt
fmt:
	mix format

.PHONY: test
test: MIX_ENV=test
test: PROPCHECK_VERBOSE=1
test:
	mix test

.PHONY: check
check:
	mix dialyzer

.PHONY: shell
shell: MIX_ENV=dev
shell: codegen
	iex -S mix

.PHONY: relsh
relsh: rel
	./_build/prod/rel/factdb/bin/factdb start_iex

.PHONY: rel
rel: codegen
	MIX_ENV=prod mix release --overwrite

package:
	sudo docker build -t factdb-build-env:latest -f docker/build_env.dockerfile .
	sudo docker run \
		--rm \
		--volume $(PWD)/_build/docker:/out \
		factdb-build-env:latest \
		cp -R /release /out
	sudo chown -R $(shell whoami): _build/docker
	sudo docker build -t $(IMAGE_NAME) -f docker/release.dockerfile .
	sudo docker push $(IMAGE_NAME)

package.run:
	sudo docker run -ti --network host  \
		-e FACTDB_GRPC_PORT=21001 \
		-e FACTDB_HEALTHZ_HTTP_PORT=21000 \
		-e FACTDB_OPENTELEMETRY_HOST=opentelemetry.foundation.svc.cluster.local \
		-e FACTDB_OPENTELEMETRY_PORT=9411 \
		$(IMAGE_NAME)

#################################################################################
# Porcelain Targets
#################################################################################

%:
	mix $@
