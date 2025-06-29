defmodule Orcasite.Radio.GraphqlClient do
  def get_resource(resource_name, feed_id, from_time, to_time) do
    case resource_name do
      :feed -> get_feeds()
      :feed_stream -> feed_streams(feed_id, from_time, to_time)
      :feed_segment -> get_feed_streams_with_segments(feed_id, from_time, to_time)
      :candidate -> get_candidates_with_detections(feed_id, from_time, to_time)
      :detection -> get_detections_with_candidates(feed_id, from_time, to_time)
      :audio_image -> get_audio_images(feed_id, from_time, to_time)
      :bout -> get_bouts(feed_id, from_time, to_time)
    end
  end

  def get_latest_resource(resource_name, feed_id, limit \\ 100) do
    case resource_name do
      :detection -> get_latest_detections_with_candidates(feed_id, limit)
      :candidate -> get_latest_candidates_with_detections(feed_id, limit)
      :audio_image -> get_latest_audio_images(feed_id, 0, limit)
      :bout -> get_latest_bouts(feed_id, 0, limit)
    end
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

  def fetch_attributes(resource) do
    name = resource |> to_string() |> String.split(".") |> List.last()

    ~s|
      {
        __type(name: "#{name}") {
          fields {
            name
          }
        }
      }
      |
    |> submit()
    |> parse_response(["data", "__type", "fields"])
    |> Enum.map(&Map.get(&1, "name"))
  end

  def camelized_public_attrs(resource) do
    local_attributes =
      resource
      |> Ash.Resource.Info.attributes()
      |> Enum.flat_map(fn
        %{name: name, public?: true} ->
          [Absinthe.Adapter.LanguageConventions.to_external_name(to_string(name), [])]

        _ ->
          []
      end)
      |> MapSet.new()

    fetchable_attributes = fetch_attributes(resource) |> MapSet.new()

    local_attributes
    |> MapSet.intersection(fetchable_attributes)
    |> MapSet.to_list()
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

            feed {
              id
            }
          }
        }
      }
    |
    |> submit()
    |> parse_response(["data", "feedStreams", "results"])
  end

  def get_feed_streams_with_segments(feed_id, from_datetime, to_datetime) do
    day_before = from_datetime |> DateTime.add(-1, :day)

    feed_stream_attrs = camelized_public_attrs(Orcasite.Radio.FeedStream)
    feed_seg_attrs = camelized_public_attrs(Orcasite.Radio.FeedSegment)

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
            #{feed_stream_attrs |> Enum.join(", ")}

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
              #{feed_seg_attrs |> Enum.join(", ")}

              feed {
                id
              }
            }
          }
        }
      }
    |
    |> submit()
    |> parse_response(["data", "feedStreams", "results"])
  end

  def get_candidates(feed_id, from_datetime, to_datetime) do
    feed_slug = get_feed_slug(feed_id)
    attrs = camelized_public_attrs(Orcasite.Radio.Candidate)

    ~s|
      {
        candidates (filter: {
          feed: {slug: {eq: "#{feed_slug}"}},
          minTime: {lessThanOrEqual: "#{DateTime.to_iso8601(to_datetime)}"},
          maxTime: {greaterThanOrEqual: "#{DateTime.to_iso8601(from_datetime)}"}
        }) {
          results {
            #{attrs |> Enum.join(", ")}
            feed {
              id
            }
          }
        }
      }
    |
    |> submit()
    |> parse_response(["data", "candidates", "results"])
  end

  def get_candidates_with_detections(feed_id, from_datetime, to_datetime) do
    feed_slug = get_feed_slug(feed_id)
    candidate_attrs = camelized_public_attrs(Orcasite.Radio.Candidate)
    detection_attrs = camelized_public_attrs(Orcasite.Radio.Detection)

    ~s|
      {
        candidates (filter: {
          feed: {slug: {eq: "#{feed_slug}"}},
          minTime: {lessThanOrEqual: "#{DateTime.to_iso8601(to_datetime)}"},
          maxTime: {greaterThanOrEqual: "#{DateTime.to_iso8601(from_datetime)}"}
        }) {
          results {
            #{candidate_attrs |> Enum.join(", ")}
            feed {
              id
            }

            detections {
              #{detection_attrs |> Enum.join(", ")}

              feed {
                id
              }
            }
          }
        }
      }
    |
    |> submit()
    |> parse_response(["data", "candidates", "results"])
  end

  def get_latest_candidates_with_detections(feed_id, limit \\ 100) do
    feed_slug = get_feed_slug(feed_id)
    candidate_attrs = camelized_public_attrs(Orcasite.Radio.Candidate)
    detection_attrs = camelized_public_attrs(Orcasite.Radio.Detection)

    ~s|
      {
        candidates (
          filter: {
            feed: {slug: {eq: "#{feed_slug}"}},
          },
          sort: {field: MIN_TIME, order: DESC},
          limit: #{limit}
        ) {
          results {
            #{candidate_attrs |> Enum.join(", ")}
            feed {
              id
            }

            detections {
              #{detection_attrs |> Enum.join(", ")}

              feed {
                id
              }
            }
          }
        }
      }
    |
    |> submit()
    |> parse_response(["data", "candidates", "results"])
  end

  def get_detections_with_candidates(feed_id, from_datetime, to_datetime) do
    feed_slug = get_feed_slug(feed_id)
    candidate_attrs = camelized_public_attrs(Orcasite.Radio.Candidate)
    detection_attrs = camelized_public_attrs(Orcasite.Radio.Detection)

    ~s|
      {
        detections (filter: {
          feed: {slug: {eq: "#{feed_slug}"}},
          and: [
            {
              timestamp: {lessThanOrEqual: "#{DateTime.to_iso8601(to_datetime)}"}
            },
            {
              timestamp: {greaterThanOrEqual: "#{DateTime.to_iso8601(from_datetime)}"}
            }
          ]
        }) {
          results {
            #{detection_attrs |> Enum.join(", ")}
            feed {
              id
            }
            candidate {
              #{candidate_attrs |> Enum.join(", ")}

              feed {
                id
              }
            }
          }
        }
      }
    |
    |> submit()
    |> parse_response(["data", "detections", "results"])
  end

  def get_latest_detections_with_candidates(feed_id, limit \\ 100) do
    feed_slug = get_feed_slug(feed_id)
    candidate_attrs = camelized_public_attrs(Orcasite.Radio.Candidate)
    detection_attrs = camelized_public_attrs(Orcasite.Radio.Detection)

    ~s|
      {
        detections (
          filter: {
            feed: {slug: {eq: "#{feed_slug}"}}
          },
          sort: {field: TIMESTAMP, order: DESC},
          limit: #{limit}
        ) {
          results {
            #{detection_attrs |> Enum.join(", ")}
            feed {
              id
            }
            candidate {
              #{candidate_attrs |> Enum.join(", ")}

              feed {
                id
              }
            }
          }
        }
      }
    |
    |> submit()
    |> parse_response(["data", "detections", "results"])
  end

  def get_audio_images(
        feed_id,
        from_datetime,
        to_datetime,
        offset \\ 0,
        limit \\ 100,
        prev_results \\ []
      ) do
    image_attrs = camelized_public_attrs(Orcasite.Radio.AudioImage)

    ~s|
      {
        audioImages (
          feedId: "#{feed_id}"
          startTime: "#{DateTime.to_iso8601(from_datetime)}"
          endTime: "#{DateTime.to_iso8601(to_datetime)}"
          offset: #{offset}
          limit: #{limit}
        ) {
          hasNextPage
          results {
            #{image_attrs |> Enum.join(", ")}
            feed {
              id
            }
          }
        }
      }
    |
    |> submit()
    |> case do
      {:ok, %{"data" => %{"audioImages" => %{"hasNextPage" => true, "results" => results}}}} ->
        get_audio_images(
          feed_id,
          from_datetime,
          to_datetime,
          offset + limit,
          limit,
          prev_results ++ results
        )

      {:ok, %{"data" => %{"audioImages" => %{"results" => results}}}} ->
        prev_results ++ results
    end
  end

  def get_latest_audio_images(
        feed_id,
        offset \\ 0,
        limit \\ 100,
        prev_results \\ []
      ) do
    image_attrs = camelized_public_attrs(Orcasite.Radio.AudioImage)
    to_datetime = DateTime.utc_now()
    from_datetime = DateTime.add(to_datetime, -1, :day)

    ~s|
      {
        audioImages (
          feedId: "#{feed_id}"
          startTime: "#{DateTime.to_iso8601(from_datetime)}"
          endTime: "#{DateTime.to_iso8601(to_datetime)}"
          offset: #{offset}
          limit: #{limit}
          sort: {field: START_TIME, order: DESC},
        ) {
          hasNextPage
          results {
            #{image_attrs |> Enum.join(", ")}
            feed {
              id
            }
          }
        }
      }
    |
    |> submit()
    |> case do
      {:ok, %{"data" => %{"audioImages" => %{"hasNextPage" => true, "results" => results}}}} ->
        get_latest_audio_images(
          feed_id,
          offset + limit,
          limit,
          prev_results ++ results
        )

      {:ok, %{"data" => %{"audioImages" => %{"results" => results}}}} ->
        prev_results ++ results
    end
  end

  def get_bouts(
        feed_id,
        from_datetime,
        to_datetime,
        offset \\ 0,
        limit \\ 100,
        prev_results \\ []
      ) do
    bout_attrs = camelized_public_attrs(Orcasite.Radio.Bout)

    ~s|
      {
        bouts (
          feedId: "#{feed_id}",
          filter: {
            startTime: {lessThanOrEqual: "#{DateTime.to_iso8601(to_datetime)}"},
            endTime: {greaterThanOrEqual: "#{DateTime.to_iso8601(from_datetime)}"}
          },
          limit: #{limit},
          offset: #{offset}
        ) {
          hasNextPage
          results {
            #{bout_attrs |> Enum.join(", ")}
            feed {
              id
            }
          }
        }
      }
    |
    |> submit()
    |> case do
      {:ok, %{"data" => %{"bouts" => %{"hasNextPage" => true, "results" => results}}}} ->
        get_bouts(
          feed_id,
          from_datetime,
          to_datetime,
          offset + limit,
          limit,
          prev_results ++ results
        )

      {:ok, %{"data" => %{"bouts" => %{"results" => results}}}} ->
        prev_results ++ results
    end
  end

  def get_latest_bouts(
        feed_id,
        offset \\ 0,
        limit \\ 100,
        prev_results \\ []
      ) do
    bout_attrs = camelized_public_attrs(Orcasite.Radio.Bout)

    ~s|
      {
        bouts (
          feedId: "#{feed_id}",
          limit: #{limit},
          offset: #{offset},
          sort: {field: START_TIME, order: DESC},
        ) {
          hasNextPage
          results {
            #{bout_attrs |> Enum.join(", ")}
            feed {
              id
            }
          }
        }
      }
    |
    |> submit()
    |> case do
      {:ok, %{"data" => %{"bouts" => %{"hasNextPage" => true, "results" => results}}}} ->
        get_latest_bouts(
          feed_id,
          offset + limit,
          limit,
          prev_results ++ results
        )

      {:ok, %{"data" => %{"bouts" => %{"results" => results}}}} ->
        prev_results ++ results
    end
  end

  defp get_feed_slug(feed_id) do
    # Need this until we figure out how to add `eq` query to AshUUID.UUID types
    %{slug: slug} = Orcasite.Radio.Feed |> Ash.get!(feed_id, authorize?: false)
    slug
  end
end
