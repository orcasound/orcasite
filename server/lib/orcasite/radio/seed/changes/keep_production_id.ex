defmodule Orcasite.Radio.Seed.Changes.KeepProductionId do
  @moduledoc """
  Gives a seeded record the id it has in production.

  Ids are not writable, so no ordinary action can set or change one. Seeding
  needs them to match production's, so that later seeds upsert onto the same
  rows and references between records line up. Actions that seed take the id as
  an `:id` argument, and this change applies it -- but only in an app with
  seeding turned on, decided at runtime because every app runs the same build.
  """
  use Ash.Resource.Change

  @impl true
  def change(changeset, _opts, _context) do
    case Ash.Changeset.get_argument(changeset, :id) do
      nil ->
        changeset

      id ->
        if Orcasite.Config.seeding_enabled?() do
          Ash.Changeset.force_change_attribute(changeset, :id, id)
        else
          Ash.Changeset.add_error(changeset,
            field: :id,
            message: "can only be set when seeding from prod"
          )
        end
    end
  end
end
