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
require Ash.Query

feeds = [
  %{
    lat_lng_string: "48.5583362, -123.1735774",
    name: "Orcasound Lab (Haro Strait)",
    node_name: "rpi_orcasound_lab",
    slug: "orcasound-lab"
  },
  %{
    lat_lng_string: "48.0336664, -122.6040035",
    name: "Bush Point",
    node_name: "rpi_bush_point",
    slug: "bush-point"
  },
  %{
    lat_lng_string: "48.135743, -122.760614",
    name: "Port Townsend",
    node_name: "rpi_port_townsend",
    slug: "port-townsend"
  },
  %{
    lat_lng_string: "47.86497296593844, -122.33393605795372",
    name: "Sunset Bay",
    node_name: "rpi_sunset_bay",
    slug: "sunset-bay"
  },
  %{
    lat_lng_string: "48.591294, -123.058779",
    name: "North San Juan Channel",
    node_name: "rpi_north_sjc",
    slug: "north-sjc"
  }
]

for attrs <- feeds do
  Orcasite.Radio.Feed
  |> Ash.Query.for_read(:read)
  |> Ash.Query.filter(slug == ^attrs.slug)
  |> Orcasite.Radio.read()
  |> case do
    {:ok, []} ->
      Orcasite.Radio.Feed
      |> Ash.Changeset.for_create(:create, attrs)
      |> Orcasite.Radio.create!(verbose?: true)
    _ ->
      nil
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
|> Orcasite.Accounts.create()
