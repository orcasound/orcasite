defmodule OrcasiteWeb.JsonApi.MaxPageSizeTest do
  @moduledoc """
  Regression tests for https://github.com/orcasound/orcasite/issues/992.

  A `page[limit]` above the action's `max_page_size` used to be silently
  clamped, returning a short page with `links.next: null` — which reads as
  "that's everything". It must be an error instead.
  """

  use OrcasiteWeb.ConnCase, async: true

  describe "page[limit] above max_page_size" do
    test "is rejected on an action with an explicit max_page_size", %{conn: conn} do
      assert %{
               "errors" => [
                 %{
                   "code" => "invalid_pagination",
                   "detail" => detail,
                   "source" => %{"parameter" => "page[limit]"},
                   "status" => "400",
                   "title" => "InvalidPagination"
                 }
               ]
             } =
               conn
               |> get("/api/json/detections?page[limit]=1001")
               |> json_response(400)

      assert detail =~ "exceeds the maximum page size of 1000"
    end

    test "is rejected on an action using Ash's default max_page_size", %{conn: conn} do
      assert %{"errors" => [%{"code" => "invalid_pagination", "detail" => detail}]} =
               conn
               |> get("/api/json/feed_segments?page[limit]=251")
               |> json_response(400)

      assert detail =~ "exceeds the maximum page size of 250"
    end
  end

  describe "page[limit] at or below max_page_size" do
    test "is served for an action with an explicit max_page_size", %{conn: conn} do
      assert %{"data" => _} =
               conn
               |> get("/api/json/detections?page[limit]=1000")
               |> json_response(200)
    end

    test "is served for an action using Ash's default max_page_size", %{conn: conn} do
      assert %{"data" => _} =
               conn
               |> get("/api/json/feed_segments?page[limit]=250")
               |> json_response(200)
    end

    test "requests without page[limit] are untouched", %{conn: conn} do
      assert %{"data" => _} =
               conn
               |> get("/api/json/detections")
               |> json_response(200)
    end
  end

  test "unmatched routes still 404 rather than being swallowed", %{conn: conn} do
    assert %{"errors" => [%{"code" => "no_route_found"}]} =
             conn
             |> get("/api/json/not_a_real_resource?page[limit]=1000000")
             |> json_response(404)
  end
end
