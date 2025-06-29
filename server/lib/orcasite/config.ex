defmodule Orcasite.Config do
  def seeding_enabled?, do: Application.get_env(:orcasite, :enable_seed_from_prod)
end
