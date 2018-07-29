defmodule OrcasiteWeb.Types.Account do
  use Absinthe.Schema.Notation

  @desc "A user"
  object :user do
    field :id, :id # clients can get the user id
    field :name, :string # clients can also ask for the name field
  end
end
