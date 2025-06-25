defmodule Orcasite.Radio.Seed do
  use Ash.Resource,
    domain: Orcasite.Radio,
    extensions: [AshGraphql.Resource, AshOban]

  resource do
    description "Non-persisted resource to seed records from specific time ranges from Orcasite prod"
  end

  attributes do
    uuid_primary_key :id

    attribute :resource, __MODULE__.Types.Resource do
      public? true
      allow_nil? false
    end

    attribute :feed_id, :string, public?: true
    attribute :start_time, :utc_datetime_usec, public?: true
    attribute :end_time, :utc_datetime_usec, public?: true
    attribute :limit, :integer, public?: true
    attribute :seeded_count, :integer, public?: true
  end

  code_interface do
    define :feeds
    define :resource
    define :all
    define :latest_resource
    define :latest
    define :delete_old
  end

  validations do
    validate {Orcasite.Radio.Validations.MaxTimeDiff,
              [
                from_date_attr: :start_time,
                to_date_attr: :end_time,
                max_interval: :timer.hours(1)
              ]},
             where: absent(:limit)

    validate fn _change, _context ->
      if Application.get_env(:orcasite, :enable_seed_from_prod) do
        :ok
      else
        {:error, message: "Seeding is disabled (this is likely the production server already)"}
      end
    end
  end

  actions do
    defaults [:read]

    action :all, {:array, :struct} do
      constraints items: [instance_of: __MODULE__]
      description "Seed feeds, then the rest of the resources"
      argument :end_time, :utc_datetime_usec, default: &DateTime.utc_now/0

      argument :start_time, :utc_datetime_usec,
        default: fn -> DateTime.utc_now() |> DateTime.add(-6, :minute) end

      run fn %{arguments: %{start_time: start_time, end_time: end_time}}, _ ->
        __MODULE__.feeds()

        feeds = Orcasite.Radio.Feed |> Ash.read!()

        seed_params =
          for feed <- feeds, resource <- [:feed_segment, :detection, :audio_image, :bout] do
            {feed, resource}
          end

        seed_params
        |> Enum.map(fn {feed, resource} -> {feed.slug, resource} end)

        seed_params
        |> Stream.map(fn {feed, resource} ->
          __MODULE__.resource!(%{
            resource: resource,
            start_time: start_time,
            end_time: end_time,
            feed_id: feed.id
          })
        end)
        |> Enum.to_list()
        |> then(&{:ok, &1})
      end
    end

    action :latest, {:array, :struct} do
      constraints items: [instance_of: __MODULE__]
      description "Seed feeds, then latest detections/candidates, audio images, bouts"
      argument :limit, :integer, allow_nil?: false, default: 100

      run fn %{arguments: %{limit: limit}}, _ ->
        __MODULE__.feeds()

        feeds = Orcasite.Radio.Feed |> Ash.read!()

        seed_params =
          for feed <- feeds, resource <- [:detection, :audio_image, :bout] do
            {feed, resource}
          end

        seed_params
        |> Enum.map(fn {feed, resource} -> {feed.slug, resource} end)

        seed_params
        |> Stream.map(fn {feed, resource} ->
          __MODULE__.latest_resource!(%{
            resource: resource,
            limit: limit,
            feed_id: feed.id
          })
        end)
        |> Enum.to_list()
        |> then(&{:ok, &1})
      end
    end

    action :delete_old, :atom do
      argument :before, :utc_datetime_usec,
        default: fn -> DateTime.utc_now() |> DateTime.add(-24 * 7, :hour) end

      run fn %{arguments: %{before: before}}, _ ->
        require Ash.Query
        feeds = Orcasite.Radio.Feed |> Ash.read!()

        delete_params =
          for feed <- feeds,
              resource_name <- [
                :detection,
                :candidate,
                :feed_segment,
                :feed_stream,
                :audio_image,
                :bout
              ] do
            {feed, to_resource(resource_name)}
          end

        delete_params
        |> Enum.map(fn {feed, resource} ->
          resource
          |> Ash.Query.filter(feed_id: feed.id, inserted_at: [less_than: before])
          |> Ash.bulk_destroy(:destroy, %{}, authorize?: false)
        end)
      end

      :ok
    end

    create :feeds do
      change set_attribute(:resource, :feed)
      change set_attribute(:start_time, &DateTime.utc_now/0)
      change set_attribute(:end_time, &DateTime.utc_now/0)

      change {__MODULE__.Changes.SeedFeeds, []}
    end

    create :latest_resource do
      argument :limit, :integer, allow_nil?: false, default: 100

      argument :resource, __MODULE__.Types.Resource do
        allow_nil? false
      end

      argument :feed_id, :string do
        allow_nil? false
        description "Local/dev server feed ID to seed relationship"
      end

      change set_attribute(:resource, arg(:resource))
      change set_attribute(:limit, arg(:limit))
      change set_attribute(:feed_id, arg(:feed_id))

      change {__MODULE__.Changes.SeedResource, [type: :latest]}
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
      change set_attribute(:feed_id, arg(:feed_id))

      change {__MODULE__.Changes.SeedResource, []}
    end
  end

  oban do
    if Application.compile_env(:orcasite, :enable_seed_from_prod, false) do
      scheduled_actions do
        # Every 5 minutes, pull the last 6 minutes of data
        schedule :sync, "*/5 * * * *" do
          action :all
          queue :seed
          worker_module_name __MODULE__.AshOban.Sync.Worker
        end

        if Application.compile_env(:orcasite, :auto_delete_seeded_records, false) do
          schedule :delete_old, "@hourly" do
            action :delete_old
            queue :seed
            worker_module_name __MODULE__.AshOban.DeleteOld.Worker
          end
        end
      end
    end
  end

  graphql do
    type :seed

    mutations do
      create :seed_feeds, :feeds
      create :seed_resource, :resource
      create :seed_latest_resource, :latest_resource
      action :seed_all, :all
    end
  end

  defp to_resource(name) do
    case name do
      :detection -> Orcasite.Radio.Detection
      :candidate -> Orcasite.Radio.Candidate
      :audio_image -> Orcasite.Radio.AudioImage
      :bout -> Orcasite.Radio.Bout
      :feed_segment -> Orcasite.Radio.FeedSegment
      :feed_stream -> Orcasite.Radio.FeedStream
    end
  end
end
