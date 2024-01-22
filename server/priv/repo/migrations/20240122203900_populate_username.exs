defmodule Orcasite.Repo.Migrations.PopulateUsername do
  use Ecto.Migration

  def up do
    execute("UPDATE users SET username = split_part(email, '@', 1) where username is null;")
    flush()
  end

  def down do
  end
end
