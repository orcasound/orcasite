defmodule Orcasite.Repo.Migrations.Subscriptions do
  use Ecto.Migration

  def change do
    create table(:subscriptions) do
      add :p256dh, :string
      add :auth, :string
      add :endpoint, :string
      add :expirationTime, :string
      
      timestamps()
    end
  end
end
  