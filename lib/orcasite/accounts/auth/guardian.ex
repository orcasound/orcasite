defmodule Orcasite.Auth.Guardian do
  @moduledoc """
  Sets up callbacks for user authentication with Guardian.
  This is the app's implementation for a token configuration.
  """
  use Guardian, otp_app: :orcasite

  alias Orcasite.Accounts

  # Used to retrieve the user
  def subject_for_token(user, _claims) do
    sub = to_string(user.id)
    {:ok, sub}
  end

  # Looks up the user from the claims
  def resource_from_claims(claims) do
    id = claims["sub"]
    user = Accounts.get_user(id)
    {:ok, user}
  end

end
