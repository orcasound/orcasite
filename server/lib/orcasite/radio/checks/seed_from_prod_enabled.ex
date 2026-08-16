defmodule Orcasite.Radio.Checks.SeedFromProdEnabled do
  @moduledoc """
  Passes only where seeding from prod is turned on for the running app.

  Read at request time rather than compile time. Deploys build one artifact and
  promote it across apps, so a compile-time answer describes the app the
  artifact was built in rather than the app serving the request. `config.exs` is
  evaluated at boot, so `Application.get_env/3` here reflects the serving app's
  own `ENABLE_SEED_FROM_PROD`.
  """
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(_opts), do: "seeding from prod is enabled for this environment"

  @impl true
  def match?(_actor, _context, _opts) do
    Application.get_env(:orcasite, :enable_seed_from_prod, false) == true
  end
end
