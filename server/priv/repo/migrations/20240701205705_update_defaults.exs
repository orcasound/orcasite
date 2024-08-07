defmodule Orcasite.Repo.Migrations.UpdateDefaults do
  @moduledoc """
  Updates resources based on their most recent snapshots.

  This file was autogenerated with `mix ash_postgres.generate_migrations`
  """

  use Ecto.Migration

  def up do
    alter table(:users) do
      modify :updated_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :inserted_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :id, :uuid, default: fragment("gen_random_uuid()")
    end

    alter table(:tokens) do
      modify :created_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :updated_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
    end

    alter table(:subscriptions) do
      modify :updated_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :inserted_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :id, :uuid, default: fragment("gen_random_uuid()")
    end

    alter table(:subscribers) do
      modify :updated_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :inserted_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :id, :uuid, default: fragment("gen_random_uuid()")
    end

    alter table(:subscriber_tokens) do
      modify :created_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :updated_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
    end

    alter table(:notifications) do
      modify :updated_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :inserted_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :id, :uuid, default: fragment("gen_random_uuid()")
    end

    alter table(:feeds) do
      modify :updated_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :inserted_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
    end

    alter table(:feed_streams) do
      modify :updated_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :inserted_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
    end

    alter table(:feed_segments) do
      modify :updated_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :inserted_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
    end

    alter table(:detections) do
      modify :updated_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :inserted_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
    end

    alter table(:candidates) do
      modify :updated_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :inserted_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
    end

    alter table(:bouts) do
      modify :updated_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :inserted_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
    end

    alter table(:bout_feed_streams) do
      modify :updated_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :inserted_at, :utc_datetime_usec, default: fragment("(now() AT TIME ZONE 'utc')")
      modify :id, :uuid, default: fragment("gen_random_uuid()")
    end
  end

  def down do
    alter table(:bout_feed_streams) do
      modify :id, :uuid, default: nil
      modify :inserted_at, :utc_datetime_usec, default: fragment("now()")
      modify :updated_at, :utc_datetime_usec, default: fragment("now()")
    end

    alter table(:bouts) do
      modify :inserted_at, :utc_datetime_usec, default: fragment("now()")
      modify :updated_at, :utc_datetime_usec, default: fragment("now()")
    end

    alter table(:candidates) do
      modify :inserted_at, :utc_datetime_usec, default: fragment("now()")
      modify :updated_at, :utc_datetime_usec, default: fragment("now()")
    end

    alter table(:detections) do
      modify :inserted_at, :utc_datetime_usec, default: fragment("now()")
      modify :updated_at, :utc_datetime_usec, default: fragment("now()")
    end

    alter table(:feed_segments) do
      modify :inserted_at, :utc_datetime_usec, default: fragment("now()")
      modify :updated_at, :utc_datetime_usec, default: fragment("now()")
    end

    alter table(:feed_streams) do
      modify :inserted_at, :utc_datetime_usec, default: fragment("now()")
      modify :updated_at, :utc_datetime_usec, default: fragment("now()")
    end

    alter table(:feeds) do
      modify :inserted_at, :utc_datetime_usec, default: fragment("now()")
      modify :updated_at, :utc_datetime_usec, default: fragment("now()")
    end

    alter table(:notifications) do
      modify :id, :uuid, default: nil
      modify :inserted_at, :utc_datetime_usec, default: fragment("now()")
      modify :updated_at, :utc_datetime_usec, default: fragment("now()")
    end

    alter table(:subscriber_tokens) do
      modify :updated_at, :utc_datetime_usec, default: fragment("now()")
      modify :created_at, :utc_datetime_usec, default: fragment("now()")
    end

    alter table(:subscribers) do
      modify :id, :uuid, default: nil
      modify :inserted_at, :utc_datetime_usec, default: fragment("now()")
      modify :updated_at, :utc_datetime_usec, default: fragment("now()")
    end

    alter table(:subscriptions) do
      modify :id, :uuid, default: nil
      modify :inserted_at, :utc_datetime_usec, default: fragment("now()")
      modify :updated_at, :utc_datetime_usec, default: fragment("now()")
    end

    alter table(:tokens) do
      modify :updated_at, :utc_datetime_usec, default: fragment("now()")
      modify :created_at, :utc_datetime_usec, default: fragment("now()")
    end

    alter table(:users) do
      modify :id, :uuid, default: nil
      modify :inserted_at, :utc_datetime_usec, default: fragment("now()")
      modify :updated_at, :utc_datetime_usec, default: fragment("now()")
    end
  end
end
