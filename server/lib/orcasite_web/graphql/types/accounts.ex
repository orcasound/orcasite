defmodule OrcasiteWeb.Graphql.Types.Accounts do
  @moduledoc false
  alias Orcasite.Accounts.User
  use Absinthe.Schema.Notation

  input_object :sign_in_with_password_input do
    field :email, non_null(:string)
    field :password, non_null(:string)
  end

  object :sign_in_with_password_result do
    field :token, :string
    field :user, :user
    field :errors, list_of(:mutation_error)
  end

  input_object :request_password_reset_input do
    field :email, non_null(:string)
  end

  object :accounts_user_mutations do
    field :sign_in_with_password, type: :sign_in_with_password_result do
      arg(:input, non_null(:sign_in_with_password_input))

      resolve(fn _, %{input: args}, _ ->
        with {:ok, user} <- User.sign_in_with_password(args) do
          {:ok, %{user: user, token: user.__metadata__.token}}
        else
          {:error, _} ->
            {:ok, %{errors: [%{code: "invalid_credentials"}]}}
        end
      end)
    end

    field :request_password_reset, type: :boolean do
      arg(:input, non_null(:request_password_reset_input))

      resolve(fn _, %{input: args}, _ ->
        User.request_password_reset_with_password(args)

        {:ok, true}
      end)
    end
  end
end
