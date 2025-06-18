defmodule Orcasite.Config do
  def seeding_enabled?, do: Application.get_env(:orcasite, :enable_prod_seed)
end
