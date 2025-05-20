defmodule Orcasite.Config do
  def seeding_enabled?, do: not Application.get_env(:orcasite, :disable_prod_seed)
end
