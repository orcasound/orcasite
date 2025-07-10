defmodule Orcasite.Radio.Bout do
  use Ash.Resource,
    domain: Orcasite.Radio,
    extensions: [AshAdmin.Resource, AshUUID, AshGraphql.Resource, AshJsonApi.Resource],
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer]

  resource do
    description "A moderator-generated time interval for a feed where there's a specific category of audio going on. Usually 10-90 minutes long."
  end

  postgres do
    table "bouts"
    repo Orcasite.Repo

    migration_defaults id: "fragment(\"uuid_generate_v7()\")"

    custom_indexes do
      index [:feed_id]
      index [:created_by_user_id]
    end
  end

  identities do
    identity :id, [:id]
  end

  attributes do
    uuid_attribute :id,
      prefix: "bout",
      public?: true,
      writable?: Orcasite.Config.seeding_enabled?()

    attribute :name, :string, public?: true
    attribute :start_time, :utc_datetime_usec, public?: true, allow_nil?: false
    attribute :end_time, :utc_datetime_usec, public?: true
    attribute :duration, :decimal, public?: true

    attribute :category, Orcasite.Types.AudioCategory do
      public? true
      allow_nil? false
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  calculations do
    calculate :export_json, :string, __MODULE__.Calculations.BoutExportJson do
      public? true
      description "JSON file for exporting the bout and its feed segments"
    end

    calculate :export_json_file_name, :string, __MODULE__.Calculations.BoutExportJsonFileName do
      public? true
    end

    calculate :export_script, :string, __MODULE__.Calculations.BoutExportScript do
      public? true
    end

    calculate :export_script_file_name,
              :string,
              __MODULE__.Calculations.BoutExportScriptFileName do
      public? true
    end
  end

  relationships do
    belongs_to :created_by_user, Orcasite.Accounts.User

    belongs_to :feed, Orcasite.Radio.Feed do
      public? true
    end

    has_many :bout_feed_streams, Orcasite.Radio.BoutFeedStream

    many_to_many :feed_streams, Orcasite.Radio.FeedStream do
      through Orcasite.Radio.BoutFeedStream
      public? true
    end

    has_many :item_tags, Orcasite.Radio.ItemTag do
      public? true
    end

    many_to_many :tags, Orcasite.Radio.Tag do
      through Orcasite.Radio.ItemTag
      public? true
    end

    has_many :feed_segments, Orcasite.Radio.FeedSegment do
      manual __MODULE__.Relationships.BoutFeedSegments
      public? true
    end
  end

  policies do
    bypass actor_attribute_equals(:admin, true) do
      authorize_if always()
    end

    bypass action_type(:read) do
      authorize_if always()
    end

    bypass action_type(:create) do
      authorize_if actor_attribute_equals(:moderator, true)
    end

    bypass action_type(:update) do
      authorize_if actor_attribute_equals(:moderator, true)
    end
  end

  actions do
    defaults [:read, :destroy]

    read :index do
      pagination do
        offset? true
        countable true
        default_limit 100
      end

      argument :feed_id, :string

      filter expr(if not is_nil(^arg(:feed_id)), do: feed_id == ^arg(:feed_id), else: true)
    end

    create :create do
      primary? true
      accept [:category, :start_time, :end_time, :name]

      argument :feed_id, :string, allow_nil?: false

      change fn changeset, _ ->
        changeset
        |> Ash.Changeset.manage_relationship(
          :feed,
          %{id: Ash.Changeset.get_argument(changeset, :feed_id)},
          type: :append
        )
      end

      change fn changeset, _ ->
        end_time = Ash.Changeset.get_argument_or_attribute(changeset, :end_time)
        start_time = Ash.Changeset.get_argument_or_attribute(changeset, :start_time)

        if start_time && end_time do
          changeset
          |> Ash.Changeset.change_attribute(
            :duration,
            DateTime.diff(end_time, start_time, :millisecond) / 1000
          )
        else
          changeset
        end
      end

      change fn
        changeset, %{actor: %Orcasite.Accounts.User{} = actor} ->
          changeset
          |> Ash.Changeset.manage_relationship(:created_by_user, actor, type: :append)

        changeset, _ ->
          changeset
      end
    end

    if Application.compile_env(:orcasite, :enable_seed_from_prod, false) do
      create :seed do
        upsert? true
        upsert_identity :id
        skip_unknown_inputs :*

        accept [:id, :category, :start_time, :end_time, :name, :duration, :feed_id]
        upsert_fields [:category, :start_time, :end_time, :name, :duration, :feed_id]
      end
    end

    update :update do
      primary? true
      accept [:category, :start_time, :end_time, :name]
      require_atomic? false

      change debug_log()

      change fn changeset, _ ->
        end_time = Ash.Changeset.get_argument_or_attribute(changeset, :end_time)
        start_time = Ash.Changeset.get_argument_or_attribute(changeset, :start_time)

        if start_time && end_time do
          changeset
          |> Ash.Changeset.change_attribute(
            :duration,
            DateTime.diff(end_time, start_time, :millisecond) / 1000
          )
        else
          changeset
        end
      end
    end
  end

  changes do
    change set_attribute(:name, expr(string_trim(arg(:name)))), where: present(arg(:name))
  end

  # validations do
  #   validate string_length(:name, min: 3), where: present(:name), before_action?: true
  # end

  json_api do
    type "bout"

    includes [:feed, :tags]

    routes do
      base "/bouts"
      index :index
    end
  end

  graphql do
    type :bout
    attribute_types feed_id: :id, feed_stream_id: :id

    queries do
      list :bouts, :index
      get :bout, :read
    end

    mutations do
      create :create_bout, :create
      update :update_bout, :update
    end
  end
end
