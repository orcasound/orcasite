defmodule Orcasite.Radio.Detection do
  use Ash.Resource,
    domain: Orcasite.Radio,
    extensions: [AshAdmin.Resource, AshUUID, AshGraphql.Resource, AshJsonApi.Resource],
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer]

  alias Orcasite.Radio.{Feed, Candidate}

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

  attributes do
    uuid_attribute :id, prefix: "det", public?: true

    attribute :source_ip, :string, public?: true
    attribute :playlist_timestamp, :integer, allow_nil?: false, public?: true
    attribute :player_offset, :decimal, allow_nil?: false, public?: true
    attribute :listener_count, :integer, public?: true
    attribute :timestamp, :utc_datetime_usec, allow_nil?: false, public?: true
    attribute :description, :string, public?: true
    attribute :visible, :boolean, default: true, public?: true

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

    bypass action_type(:create) do
      authorize_if always()
    end

    bypass action(:update_candidate) do
      authorize_if always()
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

    create :submit_detection do
      accept [
        :playlist_timestamp,
        :player_offset,
        :listener_count,
        :description,
        :category
      ]

      argument :feed_id, :string, allow_nil?: false

      argument :playlist_timestamp, :integer, allow_nil?: false
      argument :player_offset, :decimal, allow_nil?: false
      argument :listener_count, :integer, allow_nil?: true
      argument :description, :string

      argument :category, Orcasite.Types.DetectionCategory do
        allow_nil? false
      end

      argument :send_notifications, :boolean, default: true

      change set_attribute(:playlist_timestamp, arg(:playlist_timestamp))
      change set_attribute(:player_offset, arg(:player_offset))
      change set_attribute(:listener_count, arg(:listener_count))
      change set_attribute(:description, arg(:description))
      change set_attribute(:category, arg(:category))
      change set_attribute(:source_ip, context(:actor_ip))

      change manage_relationship(:feed_id, :feed, type: :append)

      change fn
        changeset, %{actor: %Orcasite.Accounts.User{} = actor} ->
          changeset
          |> Ash.Changeset.manage_relationship(:user, actor, type: :append)

        changeset, _ ->
          changeset
      end

      change fn changeset, _context ->
        playlist_timestamp = changeset |> Ash.Changeset.get_argument(:playlist_timestamp)
        player_offset = changeset |> Ash.Changeset.get_argument(:player_offset)
        category = changeset |> Ash.Changeset.get_argument(:category)

        changeset
        |> Ash.Changeset.change_attribute(
          :timestamp,
          calculate_timestamp(%{
            playlist_timestamp: playlist_timestamp,
            player_offset: player_offset
          })
        )
        |> Ash.Changeset.after_action(fn changeset, detection ->
          # Happens first
          # Find or create candidate, update detection with candidate
          candidate =
            Candidate
            |> Ash.Query.for_read(:find_nearby_candidate, %{
              timestamp: detection.timestamp,
              feed_id: detection.feed_id,
              category: category
            })
            |> Ash.read!()
            |> case do
              [] ->
                Candidate
                |> Ash.Changeset.for_create(:create, %{
                  min_time: detection.timestamp,
                  max_time: detection.timestamp,
                  detection_count: 1,
                  feed: %{id: detection.feed_id},
                  category: category
                })
                |> Ash.create!()

              [candidate] ->
                candidate
                |> Ash.Changeset.for_update(:update, %{
                  detection_count: candidate.detection_count + 1,
                  min_time: datetime_min(candidate.min_time, detection.timestamp),
                  max_time: datetime_max(candidate.max_time, detection.timestamp)
                })
                |> Ash.update!(authorize?: false)
            end

          detection
          |> Ash.Changeset.for_update(:update_candidate, %{candidate: candidate})
          |> Ash.update(authorize?: false)
        end)
        |> Ash.Changeset.after_action(fn changeset, detection ->
          # Happens second
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

      change after_action(fn _change, detection, _context ->
               Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
                 feed = detection |> Ash.load!(:feed) |> Map.get(:feed)

                 # minutes
                 buffer = 5
                 now = DateTime.utc_now()

                 start_time = detection.timestamp |> DateTime.add(-buffer, :minute)

                 plus_buffer =
                   detection.timestamp
                   |> DateTime.add(buffer, :minute)

                 end_time =
                   plus_buffer
                   |> DateTime.compare(now)
                   |> case do
                     :gt -> now
                     _ -> plus_buffer
                   end

                 feed
                 |> Ash.Changeset.for_update(:generate_spectrogram, %{
                   start_time: start_time,
                   end_time: end_time
                 })
                 |> Ash.update(authorize?: false)
               end)

               {:ok, detection}
             end)
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
      :category,
      :inserted_at
    ]

    includes [:feed, :candidate]

    routes do
      base "/detections"

      index :index
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

  defp datetime_min(time_1, time_2) do
    case DateTime.compare(time_1, time_2) do
      :lt -> time_1
      _ -> time_2
    end
  end

  defp datetime_max(time_1, time_2) do
    case DateTime.compare(time_1, time_2) do
      :gt -> time_1
      _ -> time_2
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
