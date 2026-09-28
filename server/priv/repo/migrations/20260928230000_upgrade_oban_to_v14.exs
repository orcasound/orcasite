defmodule Orcasite.Repo.Migrations.UpgradeObanToV14 do
  use Ecto.Migration

  # Oban 2.24 refuses to start against a schema older than v14.
  def up, do: Oban.Migration.up(version: 14)

  def down, do: Oban.Migration.down(version: 12)
end
