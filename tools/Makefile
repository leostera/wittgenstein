
BUILDSTAMP=$(shell date -Is | sed 's/://g; s/-//g; s/+//g')
GIT_COMMIT=$(shell git rev-parse --short HEAD)
VERSION=$(shell cat Cargo.toml | grep version | head -n 1 | awk -F' = ' '{ print $$2 }' | sed 's/[\"|,]//g')
IMAGE_NAME=registry.mainframe-0:32000/wittgenstein/tools:$(VERSION)-$(GIT_COMMIT)-$(BUILDSTAMP)

schemata: ../schemata/protobuf/wittgenstein/factdb.proto
	rm -rf ./schemata
	cp -R ../schemata .

package: schemata
	sudo docker build -t $(IMAGE_NAME) .
	sudo docker push $(IMAGE_NAME)

install: schemata
	cargo install --path .
