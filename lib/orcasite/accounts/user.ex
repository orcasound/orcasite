defmodule Orcasite.Accounts.User do
  use Ecto.Schema
  import Ecto.Changeset

  alias Orcasite.Accounts.User

  schema "users" do
    field(:email, :string, unique: true)
    field(:password_hash, :string)
    field(:first_name, :string)
    field(:last_name, :string)
    field(:admin, :boolean)
    field(:auth_token, :string)

    field(:password, :string, virtual: true)

    timestamps()
  end

  def changeset(%User{} = user, attrs) do
    user
    |> cast(attrs, [:email, :first_name, :last_name])
    |> validate_required([:email])
    |> update_change(:email, &String.downcase/1)
    |> validate_format(:email, ~r/^.+@.+$/)
    |> unique_constraint(:email, name: :users_lower_email_index)
  end

  def create_changeset(%User{} = user, attrs) do
    user
    |> changeset(attrs)
    |> cast(attrs, [:password])
    |> validate_length(:password, min: 6, max: 100)
    |> hash_password
  end

  def store_token_changeset(%User{} = user, attrs) do
    user
    |> cast(attrs, [:auth_token])
  end

  defp hash_password(%Ecto.Changeset{valid?: true, changes: %{password: password}} = changeset) do
    put_change(changeset, :password_hash, Bcrypt.hash_pwd_salt(password))
  end

  defp hash_password(changeset) do
    changeset
  end
end
