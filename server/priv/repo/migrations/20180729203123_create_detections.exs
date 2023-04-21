defmodule Orcasite.Repo.Migrations.CreateDetections do
  use Ecto.Migration

  def change do
    create table(:detections) do
      add :playlist_timestamp, :integer
      add :player_offset, :decimal
      add :source_ip, :string
      add :feed_id, references(:feeds, on_delete: :nothing)

      timestamps()
    end

    create index(:detections, [:feed_id])
  end
end
