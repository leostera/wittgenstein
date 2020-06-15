defmodule Dev.Abstractmachines.Wittgenstein.StateFactRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          entity_uri: String.t(),
          source_uri: String.t(),
          field: String.t(),
          value: String.t()
        }
  defstruct [:entity_uri, :source_uri, :field, :value]

  field :entity_uri, 2, type: :string
  field :source_uri, 3, type: :string
  field :field, 4, type: :string
  field :value, 5, type: :string
end

defmodule Dev.Abstractmachines.Wittgenstein.StateFactReply do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          response: {atom, any}
        }
  defstruct [:response]

  oneof :response, 0
  field :fact_uri, 1, type: :string, oneof: 0
  field :error, 2, type: :string, oneof: 0
end

defmodule Dev.Abstractmachines.Wittgenstein.LookupEntityRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{}
  defstruct []
end

defmodule Dev.Abstractmachines.Wittgenstein.LookupEntityReply do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{}
  defstruct []
end

defmodule Dev.Abstractmachines.Wittgenstein.Wittgenstein.Service do
  @moduledoc false
  use GRPC.Service, name: "dev.abstractmachines.wittgenstein.Wittgenstein"

  rpc :StateFact,
      Dev.Abstractmachines.Wittgenstein.StateFactRequest,
      Dev.Abstractmachines.Wittgenstein.StateFactReply

  rpc :LookupEntity,
      Dev.Abstractmachines.Wittgenstein.LookupEntityRequest,
      Dev.Abstractmachines.Wittgenstein.LookupEntityReply
end

defmodule Dev.Abstractmachines.Wittgenstein.Wittgenstein.Stub do
  @moduledoc false
  use GRPC.Stub, service: Dev.Abstractmachines.Wittgenstein.Wittgenstein.Service
end
