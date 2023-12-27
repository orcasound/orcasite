defmodule OrcasiteWeb.PageControllerTest do
  use OrcasiteWeb.ConnCase

  # TODO: Skipping this test because it requires the nextjs
  # app to be running.
  @tag :skip
  test "GET /", %{conn: conn} do
    conn = get(conn, ~p"/")
    assert html_response(conn, 200) =~ "Orcasound"
  end
end
