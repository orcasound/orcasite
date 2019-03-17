defmodule Orcasite.Repo.Migrations.CreateUsersTable do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :first_name, :string
      add :last_name, :string
      add :email, :string
      add :password, :string
      add :role, :string
      add :active, :boolean, default: true

      timestamps()
    end
  end
end
