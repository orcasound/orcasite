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
    location_point: Geo.WKT.decode!("SRID=4326;POINT(-123.1735774 48.5583362)"),
    name: "Orcasound Lab (Haro Strait)",
    node_name: "rpi_orcasound_lab",
    slug: "orcasound-lab"
  },
  %{
    location_point: Geo.WKT.decode!("SRID=4326;POINT(-122.6040035 48.0336664)"),
    name: "Bush Point",
    node_name: "rpi_bush_point",
    slug: "bush-point"
  },
  %{
    location_point: Geo.WKT.decode!("SRID=4326;POINT(-122.760614 48.135743)"),
    name: "Port Townsend",
    node_name: "rpi_port_townsend",
    slug: "port-townsend"
  },
  %{
    location_point: Geo.WKT.decode!("SRID=4326;POINT(-122.33393605795372 47.86497296593844)"),
    name: "Sunset Bay",
    node_name: "rpi_sunset_bay",
    slug: "sunset-bay"
  },
  %{
    location_point: Geo.WKT.decode!("SRID=4326;POINT(-123.058779 48.591294)"),
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

# Create admin account
strategy = AshAuthentication.Info.strategy!(Orcasite.Accounts.User, :password)
Orcasite.Accounts.User
|> Ash.Changeset.for_create(strategy.register_action_name, %{
  email: "admin@example.com",
  password: "password",
  password_confirmation: "password",
})
|> Ash.Changeset.force_change_attribute(:admin, true)
|> Orcasite.Accounts.create!()
