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

  input_object :register_with_password_input do
    field :email, non_null(:string)
    field :password, non_null(:string)
    field :password_confirmation, non_null(:string)
  end

  object :register_with_password_result do
    field :token, :string
    field :user, :user
    field :errors, list_of(:mutation_error)
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

    field :register_with_password, type: :register_with_password_result do
      arg(:input, non_null(:register_with_password_input))

      resolve(fn _, %{input: args}, _ ->
        with {:ok, user} <- User.register_with_password(args) do
          {:ok, %{user: user, token: user.__metadata__.token}}
        else
          {:error, %{errors: errors}} ->
            errors = Enum.map(errors, &AshGraphql.Error.to_error/1)

            {:ok, %{errors: errors}}
        end
      end)
    end
  end
end
