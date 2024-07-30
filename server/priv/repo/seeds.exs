# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     Orcasite.Repo.insert!(%Orcasite.SomeSchema%{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.
#
require Ash.Query

feeds = [
  # %{
  #   lat_lng_string: "48.5583362, -123.1735774",
  #   name: "Orcasound Lab (Haro Strait)",
  #   node_name: "rpi_orcasound_lab",
  #   slug: "orcasound-lab",
  #   bucket: "dev-streaming-orcasound-net",
  #   bucket_region: "us-west-2",
  # },
  %{
    lat_lng_string: "47.34922, -122.32512",
    name: "MaST Center Aquarium",
    node_name: "rpi_mast_center",
    slug: "mast-center",
    bucket: "dev-streaming-orcasound-net",
    bucket_region: "us-west-2"
  },
  # %{
  #   lat_lng_string: "48.0336664, -122.6040035",
  #   name: "Bush Point",
  #   node_name: "rpi_bush_point",
  #   slug: "bush-point",
  #   bucket: "dev-streaming-orcasound-net",
  #   bucket_region: "us-west-2",
  # },
  # %{
  #   lat_lng_string: "48.135743, -122.760614",
  #   name: "Port Townsend",
  #   node_name: "rpi_port_townsend",
  #   slug: "port-townsend",
  #   bucket: "dev-streaming-orcasound-net",
  #   bucket_region: "us-west-2",
  # },
  %{
    lat_lng_string: "47.86497296593844, -122.33393605795372",
    name: "Sunset Bay",
    node_name: "rpi_sunset_bay",
    slug: "sunset-bay",
    bucket: "dev-streaming-orcasound-net",
    bucket_region: "us-west-2"
  },
  %{
    lat_lng_string: "48.591294, -123.058779",
    name: "North San Juan Channel",
    node_name: "rpi_north_sjc",
    slug: "north-sjc",
    bucket: "dev-streaming-orcasound-net",
    bucket_region: "us-west-2"
  }
]

feeds =
  for attrs <- feeds do
    Orcasite.Radio.Feed
    |> Ash.Query.for_read(:read)
    |> Ash.Query.filter(slug == ^attrs.slug)
    |> Ash.read()
    |> case do
      {:ok, []} ->
        Orcasite.Radio.Feed
        |> Ash.Changeset.for_create(:create, attrs)
        |> Ash.create!(authorize?: false)

      {:ok, [feed | _]} ->
        feed
    end
  end

# Create admin account
strategy = AshAuthentication.Info.strategy!(Orcasite.Accounts.User, :password)

Orcasite.Accounts.User
|> Ash.Changeset.for_create(strategy.register_action_name, %{
  email: "admin@example.com",
  password: "password",
  password_confirmation: "password"
})
|> Ash.Changeset.force_change_attribute(:admin, true)
|> Ash.Changeset.force_change_attribute(:moderator, true)
|> Ash.create(authorize?: false)

[
  %{
    slug: "mast-center",
    playlist_timestamp: 1_697_873_419,
    player_offset: 5347.575374,
    listener_count: 1,
    description: "Water splashdown.",
    category: "other"
  },
  %{
    slug: "mast-center",
    playlist_timestamp: 1_698_219_018,
    player_offset: 37508.863074,
    listener_count: 2,
    description: "Ferry noise.",
    category: "vessel"
  },
  %{
    slug: "mast-center",
    playlist_timestamp: 1_698_046_218,
    player_offset: 58359.083474,
    listener_count: 2,
    description: "Distant ship noise, sounds clattery like a WA State ferry...",
    category: "vessel"
  },
  %{
    slug: "mast-center",
    playlist_timestamp: 1_697_614_218,
    player_offset: 54038.208795,
    listener_count: 1,
    description: "Power boat noise",
    category: "vessel"
  },
  %{
    slug: "mast-center",
    playlist_timestamp: 1_698_305_419,
    player_offset: 33407.46827284273,
    listener_count: 2,
    description: "Powerboat noise.",
    category: "vessel"
  },
  %{
    slug: "north-sjc",
    playlist_timestamp: 1_698_478_219,
    player_offset: 68681.244636,
    listener_count: 1,
    description:
      "Bubbling sounds, something moving and agitating/dispersing the water. A flipping sound now.",
    category: "other"
  },
  %{
    slug: "north-sjc",
    playlist_timestamp: 1_698_651_019,
    player_offset: 55512.342888,
    listener_count: 2,
    description: "Powerboat noise, likely law enforcement Knot Guilty (MMSI 338461147)",
    category: "vessel"
  },
  %{
    slug: "north-sjc",
    playlist_timestamp: 1_698_478_219,
    player_offset: 68957.393404,
    listener_count: 1,
    description: "Scraping or crunching sound",
    category: "other"
  },
  %{
    slug: "north-sjc",
    playlist_timestamp: 1_698_478_219,
    player_offset: 19747.10528763667,
    listener_count: 4,
    description: "Squeak",
    category: "other"
  },
  %{
    slug: "north-sjc",
    playlist_timestamp: 1_698_564_619,
    player_offset: 72195.849363,
    listener_count: 1,
    description: nil,
    category: "vessel"
  },
  %{
    slug: "orcasound-lab",
    playlist_timestamp: 1_698_499_818,
    player_offset: 9226.14247067801,
    listener_count: 7,
    description: "Possible KW calls in ship noise. Could be HB too…",
    category: "whale"
  },
  %{
    slug: "orcasound-lab",
    playlist_timestamp: 1_698_499_818,
    player_offset: 11603.01393371704,
    listener_count: 6,
    description: "Humpback",
    category: "whale"
  },
  %{
    slug: "orcasound-lab",
    playlist_timestamp: 1_698_521_419,
    player_offset: 11679.2351,
    listener_count: 3,
    description: "Continuous buzzing of a distant powerboat?",
    category: "vessel"
  },
  %{
    slug: "orcasound-lab",
    playlist_timestamp: 1_698_521_419,
    player_offset: 11708.3452,
    listener_count: 3,
    description: "Rev up down of powerboat motor.",
    category: "vessel"
  },
  %{
    slug: "orcasound-lab",
    playlist_timestamp: 1_698_499_818,
    player_offset: 11411.186647588003,
    listener_count: 6,
    description: "Humpbacks",
    category: "other"
  },
  %{
    slug: "port-townsend",
    playlist_timestamp: 1_698_431_442,
    player_offset: 19625.398532,
    listener_count: 23,
    description: nil,
    category: "whale"
  },
  %{
    slug: "port-townsend",
    playlist_timestamp: 1_698_431_442,
    player_offset: 21117.56658,
    listener_count: 9,
    description: "SRKW calls",
    category: "whale"
  },
  %{
    slug: "port-townsend",
    playlist_timestamp: 1_698_431_442,
    player_offset: 19376.300308946003,
    listener_count: 22,
    description: nil,
    category: "whale"
  },
  %{
    slug: "port-townsend",
    playlist_timestamp: 1_698_431_442,
    player_offset: 19674.97845850731,
    listener_count: 19,
    description: nil,
    category: "whale"
  },
  %{
    slug: "port-townsend",
    playlist_timestamp: 1_698_431_442,
    player_offset: 19988.812909963966,
    listener_count: 18,
    description: nil,
    category: "whale"
  },
  %{
    slug: "sunset-bay",
    playlist_timestamp: 1_698_476_425,
    player_offset: 2618.937792,
    listener_count: 1,
    description: "Loud train whistle!",
    category: "other"
  },
  %{
    slug: "sunset-bay",
    playlist_timestamp: 1_698_390_032,
    player_offset: 50683.820952,
    listener_count: 5,
    description: "Possible very faint call.",
    category: "whale"
  },
  %{
    slug: "sunset-bay",
    playlist_timestamp: 1_698_390_032,
    player_offset: 50842.266423,
    listener_count: 5,
    description: "Train coming with whistles.",
    category: "other"
  },
  %{
    slug: "sunset-bay",
    playlist_timestamp: 1_698_476_425,
    player_offset: 7395.749789,
    listener_count: 1,
    description: "Funny ship noise, maybe a SB 5kt pusher tug (MMSI 366921510)",
    category: "vessel"
  },
  %{
    slug: "sunset-bay",
    playlist_timestamp: 1_698_476_425,
    player_offset: 2677.146697,
    listener_count: 1,
    description: "Train passing with substantial knocking, just below 0-foot tide.",
    category: "other"
  }
]
|> Enum.map(fn attrs ->
  feeds
  |> Enum.find(fn feed -> feed.slug == attrs[:slug] end)
  |> case do
    %{id: feed_id} ->
      detection_attrs = Map.drop(attrs, [:slug])
      Orcasite.Radio.Detection
      |> Ash.Changeset.for_create(
        :submit_detection,
        Map.merge(detection_attrs, %{feed_id: feed_id, send_notifications: false})
      )
      |> Ash.create!(authorize?: false)

    _ ->
      :ok
  end
end)
