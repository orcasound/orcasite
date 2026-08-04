defmodule OrcasiteWeb.Plugs.EnforceMaxPageSize do
  @moduledoc """
  Rejects JSON:API requests whose `page[limit]` exceeds the matched action's
  `max_page_size`.

  Without this, an over-large limit is silently truncated and the response
  claims to be complete. Ash clamps the SQL `LIMIT` to `max_page_size`, but
  keeps the *requested* limit for its "is there more?" bookkeeping
  (`Ash.Actions.Read.to_page/7` splits on `page[:limit]`), so the page comes
  back short with `more?: false`. AshJsonApi then sees `Enum.count(results) <
  limit` and emits `links.next: null`. A client asking for 500 detections gets
  251 and no reason to keep paginating.

  We can't serve what was asked for, so we say so rather than under-serving it
  quietly.

  See https://github.com/orcasound/orcasite/issues/992.
  """

  @behaviour Plug

  import Plug.Conn

  @impl Plug
  def init(opts), do: Keyword.validate!(opts, domains: [])

  @impl Plug
  def call(conn, opts) do
    conn = fetch_query_params(conn)

    with {:ok, limit} <- requested_limit(conn),
         {:ok, resource, action_name} <- match_route(conn, Keyword.fetch!(opts, :domains)),
         {:ok, max_page_size} <- max_page_size(resource, action_name),
         true <- limit > max_page_size do
      send_invalid_pagination(conn, limit, max_page_size)
    else
      _ -> conn
    end
  end

  defp requested_limit(%{query_params: %{"page" => %{"limit" => limit}}}) when is_binary(limit) do
    case Integer.parse(limit) do
      {limit, ""} -> {:ok, limit}
      _ -> :error
    end
  end

  defp requested_limit(_conn), do: :error

  # Mirrors the route resolution in `AshJsonApi.Controllers.Router`. This plug
  # runs ahead of that router (which does not honor `halt/1`), so it has to find
  # the route itself to know which action's limits apply.
  defp match_route(conn, domains) do
    Enum.find_value(domains, :error, fn domain ->
      case match_domain_route(domain, conn) do
        {:ok, resource, route} ->
          {:ok, resource, route.action}

        :error ->
          domain
          |> Ash.Domain.Info.resources()
          |> Enum.filter(&(AshJsonApi.Resource in Spark.extensions(&1)))
          |> Enum.find_value(fn resource ->
            case resource.json_api_match_route(conn.method, conn.path_info) do
              {:ok, route, _params} -> {:ok, resource, route.action}
              :error -> nil
            end
          end)
      end
    end)
  end

  defp match_domain_route(domain, conn) do
    if Code.ensure_loaded?(domain) and function_exported?(domain, :json_api_match_route, 2) do
      case domain.json_api_match_route(conn.method, conn.path_info) do
        {:ok, resource, route, _params} -> {:ok, resource, route}
        :error -> :error
      end
    else
      :error
    end
  end

  # Only read actions carry `pagination`; anything else (creates, or a `related`
  # route whose action lives on the destination resource) falls through and is
  # left alone.
  defp max_page_size(resource, action_name) do
    case Ash.Resource.Info.action(resource, action_name) do
      %{pagination: %{max_page_size: max_page_size}} when is_integer(max_page_size) ->
        {:ok, max_page_size}

      _ ->
        :error
    end
  end

  defp send_invalid_pagination(conn, limit, max_page_size) do
    errors =
      AshJsonApi.Serializer.serialize_errors(nil, [
        %AshJsonApi.Error{
          id: Ash.UUID.generate(),
          status_code: 400,
          code: "invalid_pagination",
          title: "InvalidPagination",
          detail:
            "Invalid pagination: page[limit] of #{limit} exceeds the maximum page size of " <>
              "#{max_page_size} for this endpoint. Request #{max_page_size} or fewer and " <>
              "follow links.next to retrieve the rest.",
          source_parameter: "page[limit]",
          meta: %{}
        }
      ])

    conn
    |> put_resp_content_type("application/vnd.api+json")
    |> send_resp(400, errors)
    |> halt()
  end
end
