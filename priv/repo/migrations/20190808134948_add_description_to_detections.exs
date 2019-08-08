defmodule Orcasite.Repo.Migrations.AddDescriptionToDetections do
  use Ecto.Migration

  def change do
    alter table(:detections) do
      add(:description, :text)
    end
  end
end
