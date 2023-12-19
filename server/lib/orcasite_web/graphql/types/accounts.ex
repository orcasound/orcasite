defmodule OrcasiteWeb.Graphql.Types.Accounts do
  @moduledoc false
  alias Orcasite.Accounts.User
  use Absinthe.Schema.Notation

  input_object :sign_in_with_password_input do
    field :email, non_null(:string)
    field :password, non_null(:string)
  end

  object :sign_in_with_password_result do
    field :user, :user
    field :errors, list_of(:mutation_error)
  end

  input_object :request_password_reset_input do
    field :email, non_null(:string)
  end

  input_object :password_reset_input do
    field :reset_token, non_null(:string)
    field :password, non_null(:string)
    field :password_confirmation, non_null(:string)
  end

  object :password_reset_result do
    field :user, :user
    field :errors, list_of(:mutation_error)
  end

  object :accounts_user_mutations do
    field :sign_in_with_password, type: :sign_in_with_password_result do
      arg(:input, non_null(:sign_in_with_password_input))

      resolve(fn _, %{input: args}, _ ->
        with {:ok, user} <- User.sign_in_with_password(args) do
          {:ok, %{user: user}}
        else
          {:error, _} ->
            {:ok, %{errors: [%{code: "invalid_credentials"}]}}
        end
      end)

      middleware(&set_current_user/2)
    end

    field :request_password_reset, type: :boolean do
      arg(:input, non_null(:request_password_reset_input))

      resolve(fn _, %{input: args}, _ ->
        User.request_password_reset_with_password(args)

        {:ok, true}
      end)
    end

    field :reset_password, type: :password_reset_result do
      arg(:input, non_null(:password_reset_input))

      resolve(fn _,
                 %{
                   input: %{
                     password: password,
                     password_confirmation: password_confirmation,
                     reset_token: reset_token
                   }
                 },
                 _ ->
        strategy = AshAuthentication.Info.strategy!(Orcasite.Accounts.User, :password)

        with {:ok, user} <-
               AshAuthentication.Strategy.Password.Actions.reset(
                 strategy,
                 %{
                   "password" => password,
                   "password_confirmation" => password_confirmation,
                   "reset_token" => reset_token
                 },
                 []
               ) do
          {:ok, %{user: user}}
        else
          {:error, err} ->
            {:ok, %{errors: Enum.map(err.errors, &AshGraphql.Error.to_error/1)}}
        end
      end)

      middleware(&set_current_user/2)
    end

    field :sign_out, type: :boolean do
      resolve(fn _, _, _ ->
        {:ok, true}
      end)

      middleware(&clear_session/2)
    end
  end

  defp set_current_user(resolution, _) do
    with %{value: %{user: user}} <- resolution do
      Map.update!(resolution, :context, fn ctx ->
        Map.put(ctx, :current_user, user)
      end)
    end
  end

  defp clear_session(resolution, _) do
    Map.update!(resolution, :context, fn ctx ->
      ctx
      |> Map.put(:current_user, nil)
      |> Map.put(:clear_session, true)
    end)
  end
end
