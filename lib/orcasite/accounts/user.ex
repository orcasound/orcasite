defmodule Orcasite.Accounts.User do
  use Ecto.Schema
  import Ecto.Changeset

  alias Orcasite.Accounts.User

  schema "users" do
    field :first_name, :string
    field :last_name, :string
    field :email, :string, unique: true
    field :password, :string
    field :role, :string, default: "admin"
    field :active, :boolean, default: true

    timestamps()
  end

  def changeset(%User{}=user, attrs) do
    user
    |> cast(attrs, [:first_name, :last_name, :email, :password, :role, :active])
    |> validate_required([:first_name, :last_name, :email, :password, :role, :active])
    |> validate_format(:email, ~r/@/)
    |> unique_constraint(:email)
  end

end
