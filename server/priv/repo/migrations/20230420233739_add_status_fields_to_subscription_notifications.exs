defmodule Orcasite.Repo.Migrations.AddStatusFieldsToSubscriptionNotifications do
  @moduledoc """
  Updates resources based on their most recent snapshots.

  This file was autogenerated with `mix ash_postgres.generate_migrations`
  """

  use Ecto.Migration

  def up do
    alter table(:subscription_notifications) do
      add :meta, :map, default: fragment("'{}'::jsonb")
      add :channel, :text
      add :status, :text, default: "unsent"
    end
  end

  def down do
    alter table(:subscription_notifications) do
      remove :status
      remove :channel
      remove :meta
    end
  end
end
