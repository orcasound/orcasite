defmodule Orcasite.Repo.Migrations.AddListenerCountToDetections do
  use Ecto.Migration

  def change do
    alter table(:detections) do
      add :listener_count, :integer
    end
  end
end
