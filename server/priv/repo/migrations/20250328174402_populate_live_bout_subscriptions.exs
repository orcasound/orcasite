defmodule Orcasite.Repo.Migrations.PopulateLiveBoutSubscriptions do
  use Ecto.Migration

  def up do
    # Create live_bout subscriptions for each confirmed_candidate sub, skipping
    # live_bouts that already exist
    execute("""
      insert into subscriptions(name, meta, subscriber_id, event_type) (
        select s.name, s.meta, s.subscriber_id, 'live_bout'
        from subscriptions s
        left join subscriptions bout_subs on bout_subs.subscriber_id = s.subscriber_id and bout_subs.event_type = 'live_bout'
        where
          s.event_type = 'confirmed_candidate' and s.active = 't' and
          bout_subs.id is null
      )
      """ |> String.replace("\n", ""))
  end

  def down do
  end
end
