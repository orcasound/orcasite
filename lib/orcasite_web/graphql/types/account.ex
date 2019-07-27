defmodule OrcasiteWeb.Types.Account do
  use Absinthe.Schema.Notation

  @desc "A user"
  object :user do
    field(:id, :id)
    field(:first_name, :string)
    field(:last_name, :string)
    field(:email, :string)
  end
end
