defmodule Orcasite.Radio.GraphqlClient do
  def submit(query) do
    Finch.build(
      :post,
      gql_url(),
      [{"content-type", "application/json"}],
      Jason.encode!(%{
        query: query
      })
    )
    |> Finch.request(Orcasite.Finch)
    |> case do
      {:ok, %{body: body}} -> Jason.decode(body)
      resp -> resp
    end
  end

  def parse_response(response, key_path) do
    {:ok, results} = response

    results
    |> get_in(List.wrap(key_path))
  end

  def gql_url() do
    Application.get_env(:orcasite, :prod_host)
    |> case do
      "https://" <> _host = url -> url
      "http://" <> _host = url -> url
      host -> "https://" <> host
    end
    |> String.trim_trailing("/")
    |> then(&(&1 <> "/graphql"))
  end

  defp camelized_public_attrs(resource) do
    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.flat_map(fn
      %{name: name, public?: true} ->
        [Absinthe.Adapter.LanguageConventions.to_external_name(to_string(name), [])]

      _ ->
        []
    end)
  end

  ### Resources
  def get_feed(feed_slug) do
    ~s|
      {
        feed(slug: "#{feed_slug}") {
          id
          slug
        }
      }
    |
    |> submit()
    |> parse_response(["data", "feed"])
  end

  def get_feeds() do
    query = "feeds"
    attrs = camelized_public_attrs(Orcasite.Radio.Feed)

    ~s|
    {
      #{query} {
        #{Enum.join(attrs, ", ")}
      }
    }
    |
    |> submit()
    |> parse_response(["data", query])
  end

  def feed_streams(feed_id, from_datetime, to_datetime) do
    day_before = from_datetime |> DateTime.add(-1, :day)
    attrs = camelized_public_attrs(Orcasite.Radio.FeedStream)

    ~s|
      {
        feedStreams(
          feedId: "#{feed_id}",
          filter: {
            and: [
              {startTime: {lessThanOrEqual: "#{DateTime.to_iso8601(to_datetime)}"}},
              {startTime: {greaterThanOrEqual: "#{DateTime.to_iso8601(day_before)}"}}
            ],
            or: [{endTime: {isNil: true}}, {endTime: {greaterThanOrEqual: "#{DateTime.to_iso8601(from_datetime)}"}}]
          },
          sort: {field: START_TIME, order: DESC},
          limit: 2
        ) {
          count
          results {
            #{Enum.join(attrs, ", ")}
          }
        }
      }
    |
    |> submit()
    |> parse_response(["data", "feedStreams", "results"])
  end

  def get_feed_streams_with_segments(feed_id, from_datetime, to_datetime) do
    day_before = from_datetime |> DateTime.add(-1, :day)

    ~s|
      {
        feedStreams(
          feedId: "#{feed_id}",
          filter: {
            and: [
              {startTime: {lessThanOrEqual: "#{DateTime.to_iso8601(to_datetime)}"}},
              {startTime: {greaterThanOrEqual: "#{DateTime.to_iso8601(day_before)}"}}
            ],
            or: [{endTime: {isNil: true}}, {endTime: {greaterThanOrEqual: "#{DateTime.to_iso8601(from_datetime)}"}}]
          },
          sort: {field: START_TIME, order: DESC},
          limit: 2
        ) {
          count
          results {
            id
            startTime
            endTime
            duration
            bucket
            bucketRegion
            cloudfrontUrl
            playlistTimestamp
            playlistPath
            playlistM3u8Path

            feedSegments(
              filter: {
                and: [
                  {startTime: {lessThanOrEqual: "#{DateTime.to_iso8601(to_datetime)}"}},
                  {startTime: {greaterThanOrEqual: "#{DateTime.to_iso8601(day_before)}"}}
                ],
                endTime: {greaterThanOrEqual: "#{DateTime.to_iso8601(from_datetime)}"}
              },
              sort: {field: START_TIME, order: DESC},
            ) {
              startTime
              endTime
              duration
              bucket
              bucketRegion
              cloudfrontUrl
              fileName
              playlistM3u8Path
              playlistPath
              playlistTimestamp
              segmentPath
            }
          }
        }
      }
    |
    |> submit()
  end
end
