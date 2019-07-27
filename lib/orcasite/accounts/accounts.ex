defmodule Orcasite.Accounts do
  import Ecto.Query, warn: false

  alias Orcasite.Repo
  alias Orcasite.Accounts.User

  @doc """
   Creates a user.
  """
  def create_user(attrs \\ %{}) do
    %User{}
    |> User.changeset(attrs)
    |> Repo.insert()
  end

end
