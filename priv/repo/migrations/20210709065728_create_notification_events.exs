defmodule Orcasite.Repo.Migrations.CreateNotificationEvents do
  use Ecto.Migration

  def change do
    create table(:notification_events) do
      add :candidate_id, references(:candidates, on_delete: :nothing)
      add :notified_by, :string
      add :notified_at, :utc_datetime

      timestamps()
    end

    create index(:notification_events, [:candidate_id])
  end
end
