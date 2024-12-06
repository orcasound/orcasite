defmodule Orcasite.Radio.GraphqlClient do
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
                or: [{endTime: {isNil: true}}, {endTime: {greaterThanOrEqual: "#{DateTime.to_iso8601(from_datetime)}"}}]
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
end
