defmodule Orcasite.Repo.Migrations.CreateFeeds do
  use Ecto.Migration

  def up do
    execute("CREATE EXTENSION IF NOT EXISTS postgis;")
    create table(:feeds) do
      add :name, :string
      add :node_name, :string
      add :slug, :string

      timestamps()
    end

    # Add `location_point` column to store latitude and longitude in epsg4326 format
    execute("SELECT AddGeometryColumn ('feeds','location_point',4326,'POINT',2);")
  end

  def down do
    drop table(:feeds)
    execute("DROP EXTENSION IF EXISTS postgis;")
  end
end
