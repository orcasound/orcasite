defmodule Orcasite.Repo.Migrations.CreateCandidates do
  use Ecto.Migration

  def change do
    create table(:candidates) do
      add(:min_time, :utc_datetime)
      add(:max_time, :utc_datetime)
      add(:feed_id, references(:feeds, on_delete: :nothing))
      add(:detection_count, :integer)

      timestamps()
    end

    create(index(:candidates, [:feed_id]))
  end
end
