defmodule Orcasite.Accounts.ApiKey.Validations.ValidExpiration do
  use Ash.Resource.Validation

  @impl Ash.Resource.Validation
  def validate(change, _opts, _context) do
    # Expiration is in the future
    expires_at = Ash.Changeset.get_attribute(change, :expires_at)
    now = DateTime.utc_now()

    if DateTime.after?(expires_at, now) do
      :ok
    else
      {:error, "expires_at (#{inspect(expires_at)}) is not after #{inspect(now)}"}
    end
  end
end
