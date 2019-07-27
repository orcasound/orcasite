defmodule OrcasiteWeb.Guardian do
  use Guardian, otp_app: :orcasite

  def subject_for_token(resource, _claims) do
    sub = to_string(resource.id)
    {:ok, sub}
  end

  def resource_from_claims(claims) do
    user = claims["sub"] |> Orcasite.Accounts.get_user!()
    {:ok, user}
  end
end
