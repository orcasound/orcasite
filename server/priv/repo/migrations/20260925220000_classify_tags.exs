defmodule Orcasite.Repo.Migrations.ClassifyTags do
  @moduledoc """
  Give the existing tag vocabulary its `kind` and, where a tag names an animal, the
  register identifier it cites (orcasound/orcasite#1016).

  A data migration, not a seed: tags are moderator content and are created on first use
  (`ItemTag.bout_tag`), so no fixture creates them. This sets `kind`/`iri` on whichever of
  these tags exist, by name, and leaves any moderator edit made since alone -- it only
  fills a null. Applied to production by hand on 2026-09-25 with the same table, where it
  is therefore a no-op; it exists so that a restored, seeded or rebuilt database gets the
  same answers and the mapping is reviewable here rather than in an issue.

  The 16 identifiers are register entities (salish-sea/animals, edition 2026.09.6),
  matched by the register's own comparison rule (ADR-0019). `fish` is `animal` with no
  identifier on purpose: it is outside the register's scope.
  """

  use Ecto.Migration

  def up do
    execute """
    UPDATE tags AS t
    SET kind = COALESCE(t.kind, v.kind),
        iri  = COALESCE(t.iri, v.iri)
    FROM (VALUES
      ('kw', 'animal', 'SSA:0000900'),
      ('srkw', 'animal', 'SSA:0000010'),
      ('j', 'animal', 'SSA:0000020'),
      ('k', 'animal', 'SSA:0000021'),
      ('l', 'animal', 'SSA:0000022'),
      ('bigg''s', 'animal', 'SSA:0000002'),
      ('humpback', 'animal', 'SSA:0000901'),
      ('ca sea lion', 'animal', 'SSA:0000903'),
      ('seagull', 'animal', 'SSA:0000908'),
      ('t090s', 'animal', 'SSA:0000040'),
      ('t34s', 'animal', 'SSA:0002014'),
      ('t37', 'animal', 'SSA:0010082'),
      ('t38c', 'animal', 'SSA:0010101'),
      ('t419', 'animal', 'SSA:0010510'),
      ('t420', 'animal', 'SSA:0010511'),
      ('t421', 'animal', 'SSA:0010512'),
      ('fish', 'animal', NULL),
      ('call', 'signal', NULL),
      ('clicks?', 'signal', NULL),
      ('clicks-slow', 'signal', NULL),
      ('clicks-medium', 'signal', NULL),
      ('whistle', 'signal', NULL),
      ('buzz', 'signal', NULL),
      ('percussive', 'signal', NULL),
      ('squeak', 'signal', NULL),
      ('whup', 'signal', NULL),
      ('moan-ascending', 'signal', NULL),
      ('moan-descending', 'signal', NULL),
      ('bark', 'signal', NULL),
      ('excitement', 'signal', NULL),
      ('mimic-srkw', 'signal', NULL),
      ('twittering', 'signal', NULL),
      ('creak', 'signal', NULL),
      ('chirp', 'signal', NULL),
      ('snap', 'signal', NULL),
      ('grunt', 'signal', NULL),
      ('shriek', 'signal', NULL),
      ('tonal', 'signal', NULL),
      ('jingle', 'signal', NULL),
      ('s01', 'signal', NULL),
      ('s04', 'signal', NULL),
      ('s07', 'signal', NULL),
      ('s10', 'signal', NULL),
      ('s16', 'signal', NULL),
      ('s17', 'signal', NULL),
      ('s18', 'signal', NULL),
      ('s41', 'signal', NULL),
      ('wct01', 'signal', NULL),
      ('wct02', 'signal', NULL),
      ('wct06', 'signal', NULL),
      ('wct07', 'signal', NULL),
      ('wct08', 'signal', NULL),
      ('mystery', 'other', NULL),
      ('false-positive', 'other', NULL),
      ('calf', 'other', NULL),
      ('vessel', 'other', NULL),
      ('ship', 'other', NULL),
      ('boat', 'other', NULL),
      ('ferry', 'other', NULL),
      ('tug', 'other', NULL),
      ('container', 'other', NULL),
      ('bulk-carrier', 'other', NULL),
      ('noncommercial-small', 'other', NULL),
      ('lloyds', 'other', NULL),
      ('westwood', 'other', NULL),
      ('westwood olympia', 'other', NULL),
      ('westwood columbia', 'other', NULL),
      ('mmsi-563200300', 'other', NULL),
      ('mmsi-367479990', 'other', NULL),
      ('mmsi-477806000', 'other', NULL),
      ('cavitation', 'other', NULL),
      ('shaft-rub', 'other', NULL),
      ('winch', 'other', NULL),
      ('piledriving', 'other', NULL),
      ('piledriving-vibratory', 'other', NULL),
      ('train', 'other', NULL),
      ('train-whistle', 'other', NULL),
      ('airplane', 'other', NULL),
      ('60hz-hum', 'other', NULL),
      ('transmission', 'other', NULL),
      ('scuba', 'other', NULL),
      ('regulator', 'other', NULL),
      ('bubbles', 'other', NULL),
      ('water', 'other', NULL),
      ('snr-high', 'other', NULL),
      ('snr-medium', 'other', NULL),
      ('snr-low', 'other', NULL),
      ('right channel only', 'other', NULL),
      ('localization', 'other', NULL),
      ('synchronization', 'other', NULL),
      ('midnight', 'other', NULL),
      ('test', 'other', NULL),
      ('das', 'other', NULL),
      ('orcahello', 'other', NULL),
      ('s19', 'signal', NULL)
    ) AS v(name, kind, iri)
    WHERE lower(t.name) = v.name
      AND (t.kind IS NULL OR (t.iri IS NULL AND v.iri IS NOT NULL))
    """
  end

  def down do
    # Nothing to undo: this filled nulls, and a moderator may have changed them since.
    :ok
  end
end
