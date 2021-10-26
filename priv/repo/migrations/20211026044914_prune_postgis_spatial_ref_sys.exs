defmodule Orcasite.Repo.Migrations.PrunePostgisSpatialRefSys do
  use Ecto.Migration

  # Disable DDL transations because for some reason it causes the migration to
  # hang if the query has an error (which would sometimes happen on Heroku)
  @disable_ddl_transaction true

  def change do
    # Remove all the SRIDs we're not using. Currently only using 4326 (WGS84)
    # This saves >5k rows, which is important because Heroku postgres hobby has
    # a limit of 10k rows
    # See https://github.com/rgeo/activerecord-postgis-adapter/issues/273
    #
    # This migration is irreversible (on purpose), but the default SRIDs can be
    # restored by running the spatial_ref_sys.sql script that comes with postgis
    #
    # Using `query` instead of `execute` so that in case of a DB error, the
    # migration fails silently. This makes the migration "optional". It's a bit
    # weird but necessary because Heroku has problems with postgis permissions.
    # See https://github.com/orcasound/orcasite/pull/74 for details
    repo().query("DELETE FROM spatial_ref_sys WHERE srid NOT IN (4326);")
  end
end
