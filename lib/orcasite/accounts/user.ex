defmodule Orcasite.Accounts.User do
  use Ecto.Schema
  import Ecto.Changeset

   alias Orcasite.Accounts.User

   schema "users" do
    field(:first_name, :string)
    field(:last_name, :string)
    field(:email, :string, unique: true)
    field(:password_hash, :string)
    field(:password, :string, virtual: true)
    field(:password_confirmation, :string, virtual: true)
    field(:role, :string, default: "admin")
    field(:active, :boolean, default: true)

     timestamps()
  end

   def changeset(%User{}=user, attrs) do
    user
    |> cast(attrs, [:first_name, :last_name, :email, :password, :password_confirmation, :role, :active])
    |> validate_required([:first_name, :last_name, :email, :password, :password_confirmation, :role, :active])
    |> validate_format(:email, ~r/@/)
    |> update_change(:email, &String.downcase(&1))
    |> validate_length(:password, min: 8, max: 20)
    |> validate_confirmation(:password)
    |> unique_constraint(:email)
    |> hash_password
  end

  defp hash_password(%Ecto.Changeset{valid?: true, changes: %{password: password}} = changeset) do
    change(changeset, Comeonin.Argon2.add_hash(password))
  end

  defp hash_password(changeset) do
    changeset
  end

end
