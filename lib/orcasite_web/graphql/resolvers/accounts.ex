defmodule OrcasiteWeb.Resolvers.Accounts do
  alias Orcasite.Accounts

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
end
