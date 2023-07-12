defmodule Orcasite.Accounts.User do
  use Ash.Resource,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshAuthentication]


  attributes do
    uuid_primary_key :id
    attribute :email, :ci_string, allow_nil?: false
    attribute :hashed_password, :string, allow_nil?: false, sensitive?: true
    attribute :first_name, :string
    attribute :last_name, :string
    attribute :admin, :boolean

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  authentication do
    api Orcasite.Accounts

    strategies do
      password :password do
        identity_field :email
      end
    end

    tokens do
      enabled? true
      token_resource Orcasite.Accounts.Token
      signing_secret fn _, _ ->
        Application.fetch_env(:my_app, :token_signing_secret)
      end
    end
  end

  postgres do
    table "users"
    repo Orcasite.Repo
  end

  identities do
    identity :unique_email, [:email]
  end

  # use Ecto.Schema
  # import Ecto.Changeset
  # alias __MODULE__
  # schema "users" do
  #   field(:email, :string)
  #   field(:password_hash, :string)
  #   field(:first_name, :string)
  #   field(:last_name, :string)
  #   field(:admin, :boolean)
  #   field(:auth_token, :string)

  #   field(:password, :string, virtual: true)

  #   timestamps()
  # end

  # def changeset(%User{} = user, attrs) do
  #   user
  #   |> cast(attrs, [:email, :first_name, :last_name])
  #   |> validate_required([:email])
  #   |> update_change(:email, &String.downcase/1)
  #   |> validate_format(:email, ~r/^.+@.+$/)
  #   |> unique_constraint(:email, name: :users_lower_email_index)
  # end

  # def create_changeset(%User{} = user, attrs) do
  #   user
  #   |> changeset(attrs)
  #   |> password_changeset(attrs)
  # end

  # def password_changeset(user_or_changeset, attrs) do
  #   user_or_changeset
  #   |> cast(attrs, [:password])
  #   |> validate_length(:password, min: 6, max: 100)
  #   |> hash_password
  # end

  # def store_token_changeset(%User{} = user, attrs) do
  #   user
  #   |> cast(attrs, [:auth_token])
  # end

  # defp hash_password(%Ecto.Changeset{valid?: true, changes: %{password: password}} = changeset) do
  #   put_change(changeset, :password_hash, Bcrypt.hash_pwd_salt(password))
  # end

  # defp hash_password(changeset) do
  #   changeset
  # end
end
