defmodule Orcasite.Config do
  # Seeding flags are set in config/runtime.exs, so they describe the app that is
  # running rather than the one the artifact was built in.

  def seeding_enabled?, do: Application.get_env(:orcasite, :enable_seed_from_prod, false)

  def auto_update_seeded_records?,
    do: seeding_enabled?() and Application.get_env(:orcasite, :auto_update_seeded_records, false)

  def auto_delete_seeded_records?,
    do:
      auto_update_seeded_records?() and
        Application.get_env(:orcasite, :auto_delete_seeded_records, false)
end
