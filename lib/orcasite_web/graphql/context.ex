defmodule OrcasiteWeb.Context do
  @behaviour Plug

  import Plug.Conn

  alias Orcasite.Accounts

  def init(opts), do: opts

  def call(conn, _) do
    Absinthe.Plug.put_options(conn, context: build_context(conn))
  end

  @doc """
  Return the IP for the current address
  """
  def build_context(conn) do
    Map.merge(remote_ip_context(conn), current_user_context(conn))
  end

  def remote_ip_context(conn), do: %{remote_ip: conn.remote_ip}

  def current_user_context(conn) do
    with ["Bearer " <> auth_token] <- get_req_header(conn, "authorization"),
         {:ok, current_user} <- authorize(auth_token) do
      %{current_user: current_user, auth_token: auth_token}
    else
      _ -> %{}
    end
  end

  defp authorize(auth_token) do
    case Accounts.find_user_by_auth_token(auth_token) do
      nil -> {:error, "Invalid authorization token"}
      user -> {:ok, user}
    end
  end
end
