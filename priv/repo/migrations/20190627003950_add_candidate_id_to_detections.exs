defmodule Orcasite.Repo.Migrations.AddCandidateIdToDetections do
  use Ecto.Migration

  def change do
    alter table(:detections) do
      add(:candidate_id, references(:candidates, on_delete: :nothing))
    end

    create(index(:detections, [:candidate_id]))
  end
end
