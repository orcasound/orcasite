defmodule Orcasite.Accounts do
  import Ecto.Query, warn: false

  alias Orcasite.Repo
  alias Orcasite.Accounts.User

  def get_user!(id), do: Repo.get!(User, id)

  def create_user(attrs \\ %{}) do
    %User{}
    |> User.create_changeset(attrs)
    |> Repo.insert()
  end

  def update_user(%User{id: id} = user, attrs, %User{admin: admin, id: current_user_id})
      when (is_boolean(admin) and admin) or current_user_id == id do
    user
    |> User.changeset(attrs)
    |> Repo.update()
  end

  def update_password(user, password) do
    user
    |> User.password_changeset(%{password: password})
    |> Repo.update()
  end

  def login_user(user) do
    {:ok, jwt, _} = OrcasiteWeb.Guardian.encode_and_sign(user)
    {:ok, _} = store_token(user, jwt)
  end

  def store_token(%User{} = user, auth_token) do
    user
    |> User.store_token_changeset(%{auth_token: auth_token})
    |> Repo.update()
  end

  def revoke_token(%User{} = user) do
    user
    |> User.store_token_changeset(%{auth_token: nil})
    |> Repo.update()
  end

  def authenticate(%{email: email, password: password}) do
    User
    |> Repo.get_by(email: String.downcase(email))
    |> case do
      nil ->
        # Take up time
        Bcrypt.no_user_verify()
        {:error, :wrong_credentials}

      user ->
        Bcrypt.check_pass(user, password)
    end
  end
end
