defmodule OrcasiteWeb.Resolvers.Accounts do
  alias Orcasite.Accounts
  alias OrcasiteWeb.Paginated

  def current_user(_params, %{context: %{current_user: current_user}}), do: {:ok, current_user}
  def current_user(_params, _info), do: {:error, :not_authorized}

  def login(params, _info) do
    with {:ok, user} <- Accounts.authenticate(params),
         {:ok, jwt, _} <- OrcasiteWeb.Guardian.encode_and_sign(user),
         {:ok, _} <- Orcasite.Accounts.store_token(user, jwt) do
      {:ok, %{auth_token: jwt}}
    end
  end

  def logout(_args, %{context: %{current_user: current_user, auth_token: _token}}) do
    Accounts.revoke_token(current_user)
    {:ok, current_user}
  end

  def logout(_args, _info) do
    {:error, "Please log in first!"}
  end

  def create_user(params, _info) do
    case Accounts.create_user(params) do
      {:ok, user} ->
        Accounts.login_user(user)

      {:error, %Ecto.Changeset{valid?: false} = changeset} ->
        {:error,
         %{
           message: "validation",
           errors: error_response(changeset)
         }}

      {:error, errors} ->
        {:error, errors}
    end
  end

  def list_users(args, %{context: %{current_user: %{admin: true}}}) do
    {:ok, Paginated.format(Accounts.list_users(args))}
  end

  def update_user(%{id: user_id, admin: admin}, %{context: %{current_user: current_user}}) do
    with user <- Accounts.get_user!(user_id),
         {:ok, user} <- Accounts.update_user(user, %{admin: admin}, current_user) do
      {:ok, user}
    else
      error -> error
    end
  end

  defp error_response(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
  end
end
