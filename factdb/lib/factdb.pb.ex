defmodule Dev.Abstractmachines.Wittgenstein.StateFactRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          entity_uri: String.t(),
          source_uri: String.t(),
          field_uri: String.t(),
          value: String.t()
        }
  defstruct [:entity_uri, :source_uri, :field_uri, :value]

  field :entity_uri, 2, type: :string
  field :source_uri, 3, type: :string
  field :field_uri, 4, type: :string
  field :value, 5, type: :string
end

defmodule Dev.Abstractmachines.Wittgenstein.StreamedStateFactReply do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          fact_count: integer
        }
  defstruct [:fact_count]

  field :fact_count, 1, type: :int32
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

defmodule Dev.Abstractmachines.Wittgenstein.FactDB.Service do
  @moduledoc false
  use GRPC.Service, name: "dev.abstractmachines.wittgenstein.FactDB"

  rpc :StateFact,
      Dev.Abstractmachines.Wittgenstein.StateFactRequest,
      Dev.Abstractmachines.Wittgenstein.StateFactReply

  rpc :StateFacts,
      stream(Dev.Abstractmachines.Wittgenstein.StateFactRequest),
      Dev.Abstractmachines.Wittgenstein.StreamedStateFactReply

  rpc :LookupEntity,
      Dev.Abstractmachines.Wittgenstein.LookupEntityRequest,
      Dev.Abstractmachines.Wittgenstein.LookupEntityReply
end

defmodule Dev.Abstractmachines.Wittgenstein.FactDB.Stub do
  @moduledoc false
  use GRPC.Stub, service: Dev.Abstractmachines.Wittgenstein.FactDB.Service
end
