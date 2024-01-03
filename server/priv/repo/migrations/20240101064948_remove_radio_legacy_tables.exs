defmodule Orcasite.Repo.Migrations.RemoveRadioLegacyTables do
  use Ecto.Migration

  def up do
    drop table(:candidates_legacy), mode: :cascade
    drop table(:detections_legacy), mode: :cascade
    drop table(:feeds_legacy), mode: :cascade
  end

  def down do
  end
end
