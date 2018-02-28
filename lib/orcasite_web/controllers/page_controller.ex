defmodule OrcasiteWeb.PageController do
  use OrcasiteWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
