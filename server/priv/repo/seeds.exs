# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     Orcasite.Repo.insert!(%Orcasite.SomeSchema{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.
#
import Ecto.Query
alias Orcasite.RadioLegacy.Feed

feeds = [
  %{
    location_point: Geo.WKT.decode!("SRID=4326;POINT(48.5583362 -123.1735774)"),
    name: "Orcasound Lab (Haro Strait)",
    node_name: "rpi_orcasound_lab",
    slug: "orcasound-lab"
  },
  %{
    location_point: Geo.WKT.decode!("SRID=4326;POINT(48.0336664 -122.6040035)"),
    name: "Bush Point",
    node_name: "rpi_bush_point",
    slug: "bush-point"
  },
  %{
    location_point: Geo.WKT.decode!("SRID=4326;POINT(48.135743 -122.760614)"),
    name: "Port Townsend",
    node_name: "rpi_port_townsend",
    slug: "port-townsend"
  },
  %{
    location_point: Geo.WKT.decode!("SRID=4326;POINT(47.86497296593844 -122.33393605795372)"),
    name: "Sunset Bay",
    node_name: "rpi_sunset_bay",
    slug: "sunset-bay"
  },
  %{
    location_point: Geo.WKT.decode!("SRID=4326;POINT(48.591294 -123.058779)"),
    name: "North San Juan Channel",
    node_name: "rpi_north_sjc",
    slug: "north-sjc"
  }
]

for attrs <- feeds do
  from(f in Feed, where: ^Map.to_list(attrs))
  |> Orcasite.Repo.exists?()
  |> case do
    true ->
      IO.puts("[info] Feed already created")
      {:error, :already_created}

    _ ->
      %Feed{}
      |> Feed.changeset(attrs)
      |> Orcasite.Repo.insert!()
  end
end
