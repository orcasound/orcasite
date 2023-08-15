defmodule Orcasite.Repo do
  use AshPostgres.Repo, otp_app: :orcasite
  use Scrivener, page_size: 10

  @doc """
  Dynamically loads the repository url from the
  DATABASE_URL environment variable.
  """
  def init(_, opts) do
    {:ok, Keyword.put(opts, :url, System.get_env("DATABASE_URL"))}
  end

  def installed_extensions, do: ["citext", "uuid-ossp", "postgis", AshUUID.PostgresExtension]
end
