defmodule OrcasiteWeb.Types.Account do
  use Absinthe.Schema.Notation

  @desc "A user"
  object :user do
    field(:id, :id)
    field(:admin, :boolean)
    field(:first_name, :string)
    field(:last_name, :string)
    field(:auth_token, :string)
    field(:email, :string)
  end

  @desc "Pagination version of users"
  object :users do
    field(:meta, :pagination_meta)
    field(:entries, list_of(:user))
  end
end
