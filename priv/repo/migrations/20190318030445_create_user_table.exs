defmodule Orcasite.Repo.Migrations.CreateUserTable do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :first_name, :string
      add :last_name, :string
      add :email, :string
      add :password_hash, :string
      add :role, :string, default: "admin"
      add :active, :boolean, default: true

      timestamps()
    end
    create(unique_index(:users, [:email]))
  end

end
