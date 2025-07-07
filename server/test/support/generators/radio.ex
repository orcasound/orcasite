defmodule Orcasite.Generators.Radio do
  use Ash.Generator

  def feed(opts \\ []) do
    seed_generator(
      %Orcasite.Radio.Feed{
        name: StreamData.repeatedly(&Faker.Pokemon.location/0),
        node_name:
          StreamData.repeatedly(fn -> "rpi_" <> Slug.slugify(Faker.Pokemon.location()) end),
        slug: StreamData.repeatedly(fn -> Slug.slugify(Faker.Pokemon.location()) end),
        bucket: "test-audio-orcasound-net",
        location_point:
          StreamData.repeatedly(fn ->
            %Geo.Point{
              coordinates: {Faker.Address.longitude(), Faker.Address.latitude()},
              srid: 4326,
              properties: %{}
            }
          end)
      },
      overrides: opts
    )
  end

  def create_feed!(opts \\ []), do: opts |> feed() |> generate()

  def feed_stream(opts \\ []) do
    seed_generator(
      %Orcasite.Radio.FeedStream{
        feed_id: Keyword.fetch!(opts, :feed_id),
        start_time: StreamData.repeatedly(fn -> DateTime.add(DateTime.utc_now(), -6, :hour) end)
      },
      overrides: opts
    )
  end

  def create_feed_stream!(opts \\ []), do: opts |> feed_stream() |> generate()
end
