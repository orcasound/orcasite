defmodule Orcasite.Accounts.ApiKey.Validations.StillValid do
  use Ash.Resource.Validation

  @impl Ash.Resource.Validation
  def validate(change, _opts, _context) do
    %{valid: valid} = change.data |> Ash.load!(:valid, authorize?: false)

    if valid do
      :ok
    else
      {:error, "not a currently valid api key (key is expired or disabled)"}
    end
  end
end
