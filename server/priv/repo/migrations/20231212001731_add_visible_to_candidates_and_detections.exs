defmodule Orcasite.Repo.Migrations.AddVisibleToCandidatesAndDetections do
  @moduledoc """
  Updates resources based on their most recent snapshots.

  This file was autogenerated with `mix ash_postgres.generate_migrations`
  """

  use Ecto.Migration

  def up do
    alter table(:detections) do
      add :visible, :boolean, default: true
    end

    alter table(:candidates) do
      add :visible, :boolean, default: true
    end

    create index(:detections, [:visible])
    create index(:candidates, [:visible])
  end

  def down do
    alter table(:candidates) do
      remove :visible
    end

    alter table(:detections) do
      remove :visible
    end

    drop index(:candidates, [:visible])
    drop index(:detections, [:visible])
  end
end