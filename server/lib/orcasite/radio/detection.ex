defmodule Orcasite.Radio.Detection do
  use Ash.Resource,
    domain: Orcasite.Radio,
    extensions: [AshAdmin.Resource, AshUUID, AshGraphql.Resource, AshJsonApi.Resource],
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer]

  alias Orcasite.Radio.{Feed, Candidate}

  resource do
    description "A single user-submitted report of tagged audio (whale, vessel, other)"
  end

  postgres do
    table "detections"
    repo Orcasite.Repo

    custom_indexes do
      index [:playlist_timestamp]
      index [:player_offset]
      index [:timestamp]
      index [:description]
      index [:inserted_at]
      index [:category]
    end

    migration_defaults id: "fragment(\"uuid_generate_v7()\")"
  end

  identities do
    identity :id, [:id]
  end

  attributes do
    uuid_attribute :id,
      prefix: "det",
      public?: true,
      writable?: Orcasite.Config.seeding_enabled?()

    attribute :source_ip, :string, public?: true
    attribute :playlist_timestamp, :integer, allow_nil?: false, public?: true
    attribute :player_offset, :decimal, allow_nil?: false, public?: true
    attribute :listener_count, :integer, public?: true
    attribute :timestamp, :utc_datetime_usec, allow_nil?: false, public?: true
    attribute :description, :string, public?: true
    attribute :visible, :boolean, default: true, public?: true

    attribute :source, __MODULE__.Types.Source do
      default :human
      allow_nil? false
      public? true
    end

    attribute :category, Orcasite.Types.DetectionCategory do
      # TODO: Figure out what to do with old detections
      # without a category
      # allow_nil? false
      public? true
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  calculations do
    calculate :uuid, :string, Orcasite.Radio.Calculations.DecodeUUID
  end

  relationships do
    belongs_to :candidate, Candidate, public?: true

    belongs_to :feed, Feed do
      public? true
      writable? true
      allow_nil? false
    end

    belongs_to :user, Orcasite.Accounts.User
  end

  policies do
    bypass actor_attribute_equals(:admin, true) do
      authorize_if always()
    end

    bypass action_type(:read) do
      authorize_if always()
    end

    bypass action(:seed) do
      authorize_if always()
    end

    bypass action(:submit_detection) do
      authorize_if always()
    end

    bypass action(:submit_machine_detection) do
      authorize_if actor_attribute_equals(:detection_bot, true)
    end

    policy changing_attributes([:visible]) do
      authorize_if actor_attribute_equals(:moderator, true)
    end

    policy expr(is_nil(visible) or not visible) do
      authorize_if actor_attribute_equals(:moderator, true)
    end
  end

  field_policies do
    field_policy [:source_ip] do
      authorize_if actor_attribute_equals(:admin, true)
      authorize_if actor_attribute_equals(:moderator, true)
    end

    field_policy :* do
      authorize_if always()
    end
  end

  code_interface do
    define :submit_detection
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
      prepare build(load: [:uuid], sort: [inserted_at: :desc])
    end

    read :by_category do
      pagination do
        offset? true
        countable true
        default_limit 100
      end

      argument :category, Orcasite.Types.DetectionCategory do
        allow_nil? false
      end

      prepare build(load: [:uuid], sort: [inserted_at: :desc])

      filter expr(category == ^arg(:category))
    end

    update :update do
      primary? true
      argument :candidate, :map
      require_atomic? false
      accept [:description, :visible, :category]

      change manage_relationship(:candidate, type: :append)
    end

    update :update_candidate do
      argument :candidate, :map
      require_atomic? false

      change manage_relationship(:candidate, type: :append)
    end

    update :set_visible do
      accept [:visible]
      require_atomic? false
      argument :visible, :boolean, default: true

      change set_attribute(:visible, arg(:visible))

      change fn changeset, _ ->
        changeset
        |> Ash.Changeset.after_action(fn changeset, detection ->
          candidate =
            detection
            |> Ash.load!(candidate: [:detections])
            |> Map.get(:candidate)

          # If all detections are hidden, make the candidate hidden
          candidate
          |> Ash.Changeset.for_update(:update, %{
            visible: !Enum.all?(candidate.detections, &(!&1.visible))
          })
          |> Ash.update!(authorize?: false)

          {:ok, detection}
        end)
      end
    end

    create :create do
      primary? true
      argument :candidate, :map
      argument :feed, :map

      change manage_relationship(:candidate, type: :append)
      change manage_relationship(:feed, type: :append)
    end

    if Application.compile_env(:orcasite, :enable_seed_from_prod, false) do
      create :seed do
        upsert? true
        upsert_identity :id

        skip_unknown_inputs :*

        accept [
          :id,
          :source_ip,
          :source,
          :playlist_timestamp,
          :player_offset,
          :listener_count,
          :timestamp,
          :description,
          :visible,
          :category
        ]

        upsert_fields [
          :source_ip,
          :source,
          :playlist_timestamp,
          :player_offset,
          :listener_count,
          :timestamp,
          :description,
          :visible,
          :category
        ]

        argument :feed, :map
        argument :candidate, :map

        change manage_relationship(:feed, type: :append)
        change manage_relationship(:candidate, on_lookup: :relate, on_no_match: {:create, :seed})
      end
    end

    create :submit_detection do
      accept [
        :playlist_timestamp,
        :player_offset,
        :listener_count,
        :description,
        :category
      ]

      argument :feed_id, :string, allow_nil?: false

      argument :send_notifications, :boolean, default: true

      change set_attribute(:source_ip, context(:actor_ip))

      change manage_relationship(:feed_id, :feed, type: :append)

      change __MODULE__.Changes.UpdateCandidate

      change fn
        changeset, %{actor: %Orcasite.Accounts.User{} = actor} ->
          changeset
          |> Ash.Changeset.manage_relationship(:user, actor, type: :append)

        changeset, _ ->
          changeset
      end

      change fn changeset, _context ->
        playlist_timestamp =
          changeset |> Ash.Changeset.get_argument_or_attribute(:playlist_timestamp)

        player_offset = changeset |> Ash.Changeset.get_argument_or_attribute(:player_offset)

        changeset
        |> Ash.Changeset.change_attribute(
          :timestamp,
          calculate_timestamp(%{
            playlist_timestamp: playlist_timestamp,
            player_offset: player_offset
          })
        )
        |> Ash.Changeset.after_action(fn changeset, detection ->
          detection =
            detection
            |> Ash.load!([:feed, :candidate])

          if Ash.Changeset.get_argument(changeset, :send_notifications) do
            Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
              Orcasite.Notifications.Notification.notify_new_detection(
                detection,
                detection.candidate,
                detection.feed,
                authorize?: false
              )
            end)
          end

          {:ok, detection}
        end)
      end

      change __MODULE__.Changes.GenerateSpectrograms
    end

    create :submit_machine_detection do
      accept [:timestamp, :feed_id, :description]

      change set_attribute(:user_id, actor(:id))
      change set_attribute(:source_ip, context(:actor_ip))
      change set_attribute(:category, :whale)
      change set_attribute(:source, :machine)
      change set_attribute(:listener_count, 0)

      change before_action(fn change, _context ->
               feed_stream =
                 Orcasite.Radio.FeedStream.for_timestamp!(
                   %{
                     feed_id: change.attributes.feed_id,
                     timestamp: change.attributes.timestamp
                   },
                   authorize?: false
                 )

               # Get feed stream for detection timestamp, fetch
               # playlist timestamp and calculat player offset
               change
               |> Ash.Changeset.force_change_attributes(%{
                 playlist_timestamp: DateTime.to_unix(feed_stream.start_time),
                 player_offset:
                   DateTime.diff(
                     change.attributes.timestamp,
                     feed_stream.start_time,
                     :millisecond
                   ) /
                     1000
               })
             end)

      change __MODULE__.Changes.UpdateCandidate
      change __MODULE__.Changes.GenerateSpectrograms
    end
  end

  admin do
    table_columns [
      :id,
      :feed_id,
      :playlist_timestamp,
      :player_offset,
      :listener_count,
      :description,
      :candidate_id,
      :inserted_at
    ]
  end

  json_api do
    type "detection"

    default_fields [
      :id,
      :playlist_timestamp,
      :player_offset,
      :listener_count,
      :timestamp,
      :description,
      :source,
      :category,
      :inserted_at
    ]

    includes [:feed, :candidate]

    routes do
      base "/detections"

      index :index
      post :submit_machine_detection
    end
  end

  graphql do
    type :detection
    attribute_types candidate_id: :id, feed_id: :id

    queries do
      get :detection, :read
      list :detections, :index
    end

    mutations do
      create :submit_detection, :submit_detection
      update :set_detection_visible, :set_visible
    end
  end

  defp calculate_timestamp(%{playlist_timestamp: ts, player_offset: offset})
       when is_nil(ts) or is_nil(offset),
       do: nil

  defp calculate_timestamp(%{
         playlist_timestamp: playlist_timestamp,
         player_offset: player_offset
       }) do
    epoch =
      playlist_timestamp
      |> case do
        ts when is_integer(ts) -> ts
        ts when is_binary(ts) -> String.to_integer(ts)
      end

    offset =
      player_offset
      |> case do
        %Decimal{} = offset -> round(Decimal.to_float(offset))
        offset when is_float(offset) -> round(offset)
      end

    epoch
    |> DateTime.from_unix!()
    |> DateTime.add(offset)
  end
end
