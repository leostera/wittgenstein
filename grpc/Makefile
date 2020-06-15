
setup:
	mix escript.install hex protobuf

codegen:
	protoc -I priv/protos --elixir_out=plugins=grpc:./lib/ priv/protos/*.proto
