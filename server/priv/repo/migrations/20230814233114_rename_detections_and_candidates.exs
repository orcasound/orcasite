defmodule Orcasite.Repo.Migrations.RenameDetectionsAndCandidates do
  use Ecto.Migration

  def up do
    rename table("detections"), to: table("detections_legacy")
    rename table("candidates"), to: table("candidates_legacy")
  end

  def down do
    rename table("detections_legacy"), to: table("detections")
    rename table("candidates_legacy"), to: table("candidates")
  end
end
