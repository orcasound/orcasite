defmodule Orcasite.Repo.Migrations.RenameDetectionsAndCandidates do
  use Ecto.Migration

  def up do
    rename table("detections"), to: table("detections_legacy")
    rename table("candidates"), to: table("candidates_legacy")
    rename table("feeds"), to: table("feeds_legacy")

    execute("alter index feeds_unique_slug_index rename to feeds_legacy_unique_slug_index;")

  end

  def down do
    rename table("detections_legacy"), to: table("detections")
    rename table("candidates_legacy"), to: table("candidates")
    rename table("feeds_legacy"), to: table("feeds")
    execute("alter index feeds_legacy_unique_slug_index rename to feeds_unique_slug_index;")
  end
end
