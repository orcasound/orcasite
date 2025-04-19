defmodule Orcasite.Repo do
  use AshPostgres.Repo, otp_app: :orcasite

  @doc """
  Dynamically loads the repository url from the
  DATABASE_URL environment variable.
  """
  def init(_, opts) do
    {:ok, Keyword.put(opts, :url, System.get_env("DATABASE_URL"))}
  end

  def installed_extensions do
    [
      "citext",
      "uuid-ossp",
      "postgis",
      AshUUID.PostgresExtension,
      "pg_stat_statements",
      "ash-functions",
      "pg_trgm"
    ]
  end

  def min_pg_version() do
    %Version{major: 14, minor: 0, patch: 0}
  end
end
