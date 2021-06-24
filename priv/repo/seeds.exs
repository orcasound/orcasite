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
alias Orcasite.Radio.{Feed, Detection, Candidate}

feedattrs = %{
  location_point: Geo.WKT.decode!("SRID=4326;POINT(47.60621 -122.33207)"),
  name: "Orcasound Lab (Haro Strait)",
  node_name: "rpi_orcasound_lab",
  slug: "orcasound-lab"
}

detectionattrs = %{
  playlist_timestamp: 1623958233,
  player_offset: Decimal.new("21421.678924"),
  source_ip: "127.0.0.0",
  feed_id: 1
}

candidateattrs = %{
  min_time: DateTime.utc_now(),
  max_time: DateTime.utc_now(),
  feed_id: 1,
  detection_count: 1
}

from(f in Feed, where: ^Map.to_list(feedattrs))
|> Orcasite.Repo.exists?()
|> case do
  true ->
    IO.puts("[info] Feed already created")
    {:error, :already_created}

  _ ->
    %Feed{}
    |> Feed.changeset(feedattrs)
    |> Orcasite.Repo.insert!()
end

from(f in Detection, where: ^Map.to_list(detectionattrs))
|> Orcasite.Repo.exists?()
|> case do
  true ->
    IO.puts("[info] Detection already has dummy values")
    {:error, :already_created}

  _ ->
    %Detection{}
    |> Detection.changeset(detectionattrs)
    |> Orcasite.Repo.insert!()
end

from(f in Candidate, where: ^Map.to_list(candidateattrs))
|> Orcasite.Repo.exists?()
|> case do
  true ->
    IO.puts("[info] Candidate already has dummy values")
    {:error, :already_created}

  _ ->
    %Candidate{}
    |> Candidate.changeset(candidateattrs)
    |> Orcasite.Repo.insert!()
end