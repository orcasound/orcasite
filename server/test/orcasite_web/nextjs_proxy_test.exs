defmodule OrcasiteWeb.NextjsProxyTest do
  use OrcasiteWeb.ConnCase

  # Next isn't running in tests, so the proxy fails to reach it and answers
  # 502. That still shows the request got past the pipeline to the proxy.
  test "passes a request that accepts only images on to Next", %{conn: conn} do
    conn =
      conn
      |> put_req_header("accept", "image/png")
      |> get("/api/og/feed/orcasound-lab")

    assert conn.status == 502
  end
end
