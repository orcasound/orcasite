defmodule Orcasite.Repo.Migrations.AddProfileAndPreferencesToUsers do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add :is_scientist, :boolean, default: false, null: false
      add :organization, :string
      add :volunteering, :boolean, default: false, null: false
      add :user_testing, :boolean, default: false, null: false
      add :newsletter, :boolean, default: false, null: false
    end
  end
end
