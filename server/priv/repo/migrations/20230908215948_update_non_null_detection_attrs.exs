defmodule Orcasite.Repo.Migrations.UpdateNonNullDetectionAttrs do
  @moduledoc """
  Updates resources based on their most recent snapshots.

  This file was autogenerated with `mix ash_postgres.generate_migrations`
  """

  use Ecto.Migration

  def up do
    alter table(:detections) do
      modify :timestamp, :utc_datetime_usec, null: false
      modify :player_offset, :decimal, null: false
      modify :playlist_timestamp, :bigint, null: false
    end
  end

  def down do
    alter table(:detections) do
      modify :playlist_timestamp, :bigint, null: true
      modify :player_offset, :decimal, null: true
      modify :timestamp, :utc_datetime_usec, null: true
    end
  end
end
