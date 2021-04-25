defmodule Orcasite.Repo.Migrations.AddTimestampToDetections do
  use Ecto.Migration

  def change do
    alter table(:detections) do
      add :timestamp, :utc_datetime
    end
  end
end
