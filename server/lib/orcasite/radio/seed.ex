defmodule Orcasite.Radio.Seed do
  use Ash.Resource,
    domain: Orcasite.Radio,
    extensions: [AshGraphql.Resource]

  resource do
    description "Non-persisted resource to seed records from specific time ranges from Orcasite prod"
  end

  attributes do
    uuid_primary_key :id

    attribute :resource, __MODULE__.Types.Resource do
      public? true
      allow_nil? false
    end

    attribute :start_time, :utc_datetime_usec, allow_nil?: false, public?: true
    attribute :end_time, :utc_datetime_usec, allow_nil?: false, public?: true
    attribute :seeded_count, :integer, public?: true
  end

  code_interface do
    define :feeds
  end

  validations do
    validate {Orcasite.Radio.Validations.MaxTimeDiff,
              [
                from_date_attr: :start_time,
                to_date_attr: :end_time,
                max_interval: :timer.hours(1)
              ]}

    validate fn _change, _context ->
      if Application.get_env(:orcasite, :disable_prod_seed) do
        {:error, message: "Seeding is disabled (this is likely the production server already)"}
      else
        :ok
      end
    end
  end

  actions do
    create :feeds do
      change set_attribute(:resource, :feed)
      change set_attribute(:start_time, &DateTime.utc_now/0)
      change set_attribute(:end_time, &DateTime.utc_now/0)

      change {__MODULE__.Changes.SeedFeeds, []}
    end

    create :resource do
      argument :start_time, :utc_datetime_usec,
        default: fn -> DateTime.add(DateTime.utc_now(), -1, :hour) end

      argument :end_time, :utc_datetime_usec, default: &DateTime.utc_now/0

      argument :resource, __MODULE__.Types.Resource do
        allow_nil? false
      end

      argument :feed_id, :string do
        allow_nil? false
        description "Local/dev server feed ID to seed relationship"
      end

      change set_attribute(:resource, arg(:resource))
      change set_attribute(:start_time, arg(:start_time))
      change set_attribute(:end_time, arg(:end_time))

      change {__MODULE__.Changes.SeedResource, []}
    end
  end

  graphql do
    type :seed

    mutations do
      create :seed_feeds, :feeds
      create :seed_resource, :resource
    end
  end
end
