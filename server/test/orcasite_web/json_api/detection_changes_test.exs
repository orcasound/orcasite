defmodule OrcasiteWeb.JsonApi.DetectionChangesTest do
  # A reader follows detections by what changed: it reads /detections/changes, which is
  # ordered by updated_at and id and paged by a cursor, and filters on updated_at.
  use OrcasiteWeb.ConnCase, async: true

  alias Orcasite.Radio.Detection

  setup do
    feed = Orcasite.Generators.Radio.create_feed!()
    Orcasite.Generators.Radio.create_feed_stream!(feed_id: feed.id)

    detections =
      for i <- 1..5 do
        Detection.submit_detection!(%{
          category: :whale,
          feed_id: feed.id,
          playlist_timestamp: DateTime.to_unix(DateTime.utc_now()),
          player_offset: i,
          description: "Detection #{i}",
          listener_count: 1,
          send_notifications: false
        })
      end

    [feed: feed, detections: detections]
  end

  defp get_json(conn, path) do
    conn
    |> put_req_header("accept", "application/vnd.api+json")
    |> get(path)
    |> json_response(200)
  end

  @changes "/api/json/detections/changes?page[limit]=2"

  # Reads one page and returns its ids and its links.next, which carries the cursor
  # (page[after]) for the page that follows. A record's own cursor isn't returned:
  # AshJsonApi looks for it in the record's `metadata` rather than `__metadata__`.
  defp read_page(conn, path) do
    %{"data" => data, "links" => links} = get_json(conn, path)
    {Enum.map(data, & &1["id"]), links["next"]}
  end

  defp read_all(conn, path \\ @changes, ids \\ []) do
    case read_page(conn, path) do
      {page, nil} -> ids ++ page
      {page, next} -> read_all(conn, relative(next), ids ++ page)
    end
  end

  defp relative(url) do
    %URI{path: path, query: query} = URI.parse(url)
    path <> "?" <> query
  end

  defp ids_by_updated_at(detections) do
    detections
    |> Enum.sort_by(&{&1.updated_at, &1.id}, fn {a, ida}, {b, idb} ->
      case DateTime.compare(a, b) do
        :lt -> true
        :gt -> false
        :eq -> ida <= idb
      end
    end)
    |> Enum.map(& &1.id)
  end

  test "updated_at is returned and can be requested in a sparse fieldset", %{conn: conn} do
    %{"data" => [%{"attributes" => attributes} | _]} =
      get_json(conn, "/api/json/detections/changes?fields[detection]=updated_at,visible")

    assert %{"updated_at" => updated_at, "visible" => true} = attributes
    assert {:ok, _, _} = DateTime.from_iso8601(updated_at)
  end

  test "a cursor reads every detection once, in order of updated_at and id",
       %{conn: conn, detections: detections} do
    assert read_all(conn) == ids_by_updated_at(detections)
  end

  test "a cursor skips no detection when another is removed and one added between pages",
       %{conn: conn, detections: detections, feed: feed} do
    [first, second | rest] = ids_by_updated_at(detections)

    {[^first, ^second], next} = read_page(conn, @changes)
    assert next =~ "page[after]="

    detections |> Enum.find(&(&1.id == first)) |> Ash.destroy!(authorize?: false)

    added =
      Detection.submit_detection!(%{
        category: :whale,
        feed_id: feed.id,
        playlist_timestamp: DateTime.to_unix(DateTime.utc_now()),
        player_offset: 9,
        listener_count: 1,
        send_notifications: false
      })

    assert read_all(conn, relative(next)) == rest ++ [added.id]
  end

  test "filtering on updated_at returns only the detections changed since",
       %{conn: conn, detections: detections} do
    [_, second | _] = sorted = Enum.sort_by(detections, & &1.updated_at, DateTime)
    since = DateTime.to_iso8601(hd(sorted).updated_at)

    %{"data" => data} =
      get_json(
        conn,
        "/api/json/detections/changes?filter[updated_at][greater_than]=#{URI.encode_www_form(since)}"
      )

    returned = Enum.map(data, & &1["id"])
    refute hd(sorted).id in returned
    assert second.id in returned
    assert length(returned) == length(detections) - 1
  end

  test "a hidden detection comes back with visible false by default",
       %{conn: conn, detections: [detection | _]} do
    detection
    |> Ash.Changeset.for_update(:set_visible, %{visible: false})
    |> Ash.update!(authorize?: false)

    %{"data" => data} = get_json(conn, "/api/json/detections/changes")

    assert %{"attributes" => %{"visible" => false}} = Enum.find(data, &(&1["id"] == detection.id))
  end

  test "hiding a detection and changing its category bump updated_at",
       %{detections: [detection | _]} do
    hidden =
      detection
      |> Ash.Changeset.for_update(:set_visible, %{visible: false})
      |> Ash.update!(authorize?: false)

    assert DateTime.compare(hidden.updated_at, detection.updated_at) == :gt

    recategorized =
      hidden
      |> Ash.Changeset.for_update(:update, %{category: :vessel})
      |> Ash.update!(authorize?: false)

    assert DateTime.compare(recategorized.updated_at, hidden.updated_at) == :gt
  end

  test "re-submitting a machine detection bumps updated_at", %{feed: feed} do
    submit = fn timestamp ->
      Detection
      |> Ash.Changeset.for_create(:submit_machine_detection, %{
        timestamp: timestamp,
        feed_id: feed.id,
        description: "Machine detection",
        idempotency_key: "orcahello-1"
      })
      |> Ash.create!(authorize?: false)
    end

    capture_log(fn ->
      first = submit.(DateTime.utc_now())
      second = submit.(DateTime.add(DateTime.utc_now(), -1, :minute))

      assert second.id == first.id
      assert DateTime.compare(second.updated_at, first.updated_at) == :gt
    end)
  end
end
