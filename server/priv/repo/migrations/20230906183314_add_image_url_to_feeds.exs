defmodule Orcasite.Repo.Migrations.AddImageUrlToFeeds do
  @moduledoc """
  Updates resources based on their most recent snapshots.

  This file was autogenerated with `mix ash_postgres.generate_migrations`
  """

  use Ecto.Migration

  def up do
    alter table(:feeds) do
      add :image_url, :text, default: ""
    end

    create index(:detections, ["inserted_at"])

    create index(:candidates, ["inserted_at"])
  end

  def down do
    drop_if_exists index(:candidates, ["inserted_at"], name: "candidates_inserted_at_index")

    drop_if_exists index(:detections, ["inserted_at"], name: "detections_inserted_at_index")

    alter table(:feeds) do
      remove :image_url
    end
  end
end