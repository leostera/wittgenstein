.PHONY: codegen
codegen:
	python -m grpc_tools.protoc \
		-I../ \
		--python_out=. \
		--grpc_python_out=. \
		./../schemata/protobuf/wittgenstein/factdb.proto
