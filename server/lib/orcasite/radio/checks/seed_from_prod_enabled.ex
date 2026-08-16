defmodule Orcasite.Radio.Checks.SeedFromProdEnabled do
  @moduledoc """
  Passes only where seeding from prod is turned on for the running app.

  Read at request time rather than compile time. Deploys build one artifact and
  promote it across apps, so a compile-time answer describes the app the
  artifact was built in rather than the app serving the request.

  The value is set in `config/runtime.exs`, which runs at boot under both Mix
  and a release. `config.exs` sets it too, for the `Application.compile_env/3`
  calls that decide whether the seed actions exist at all -- but that copy is
  frozen into the artifact under a release, so it must not be what authorizes a
  request.
  """
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(_opts), do: "seeding from prod is enabled for this environment"

  @impl true
  def match?(_actor, _context, _opts) do
    Application.get_env(:orcasite, :enable_seed_from_prod, false) == true
  end
end
