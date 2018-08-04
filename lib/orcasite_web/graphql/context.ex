defmodule OrcasiteWeb.Context do
  @behaviour Plug

  def init(opts), do: opts

  def call(conn, _) do
    context = build_context(conn)
    Absinthe.Plug.put_options(conn, context: context)
  end

  @doc """
  Return the IP for the current address
  """
  def build_context(conn) do
    %{remote_ip: conn.remote_ip}
  end
end
