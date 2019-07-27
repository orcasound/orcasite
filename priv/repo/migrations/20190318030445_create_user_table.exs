defmodule Orcasite.Repo.Migrations.CreateUserTable do
  use Ecto.Migration

  def change do
    create table(:users) do
      add(:first_name, :string)
      add(:last_name, :string)
      add(:email, :string)
      add(:password_hash, :string)
      add(:admin, :boolean, default: false)
      add(:auth_token, :text)

      timestamps()
    end

    create(index(:users, ["lower(email)"], unique: true))
  end
end
