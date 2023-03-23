defmodule OrcasiteWeb.Auth do
  import Plug.Conn

  def load_current_user(conn, _) do
    conn
    |> assign(:current_user, Guardian.Plug.current_resource(conn))
  end
end
