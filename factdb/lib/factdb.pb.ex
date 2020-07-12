defmodule Dev.Abstractmachines.Wittgenstein.ProjectionDescription do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{}
  defstruct []
end

defmodule Dev.Abstractmachines.Wittgenstein.ProjectedEntity.FieldsEntry do
  @moduledoc false
  use Protobuf, map: true, syntax: :proto3

  @type t :: %__MODULE__{
          key: String.t(),
          value: String.t()
        }
  defstruct [:key, :value]

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule Dev.Abstractmachines.Wittgenstein.ProjectedEntity do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          entity_uri: String.t(),
          fields: %{String.t() => String.t()}
        }
  defstruct [:entity_uri, :fields]

  field :entity_uri, 1, type: :string

  field :fields, 2,
    repeated: true,
    type: Dev.Abstractmachines.Wittgenstein.ProjectedEntity.FieldsEntry,
    map: true
end

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
      stream(Dev.Abstractmachines.Wittgenstein.StateFactReply)

  rpc :LookupEntity,
      Dev.Abstractmachines.Wittgenstein.LookupEntityRequest,
      Dev.Abstractmachines.Wittgenstein.LookupEntityReply

  rpc :Project,
      Dev.Abstractmachines.Wittgenstein.ProjectionDescription,
      stream(Dev.Abstractmachines.Wittgenstein.ProjectedEntity)
end

defmodule Dev.Abstractmachines.Wittgenstein.FactDB.Stub do
  @moduledoc false
  use GRPC.Stub, service: Dev.Abstractmachines.Wittgenstein.FactDB.Service
end
