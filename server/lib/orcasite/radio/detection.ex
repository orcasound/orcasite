defmodule Orcasite.Radio.Detection do
  use Ash.Resource,
    extensions: [AshAdmin.Resource, AshUUID, AshGraphql.Resource],
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer]

  alias Orcasite.Radio.Category
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
    uuid_attribute(:id, prefix: "det")

    attribute :source_ip, :string
    attribute :playlist_timestamp, :integer, allow_nil?: false
    attribute :player_offset, :decimal, allow_nil?: false
    attribute :listener_count, :integer
    attribute :timestamp, :utc_datetime_usec, allow_nil?: false
    attribute :description, :string
    attribute :visible, :boolean, default: true

    attribute :category, :atom do
      # TODO: Figure out what to do with old detections
      # without a category
      # allow_nil? false
      constraints one_of: Category.list()
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  calculations do
    calculate :uuid, :string, Orcasite.Radio.Calculations.DecodeUUID
  end

  relationships do
    belongs_to :candidate, Candidate
    belongs_to :feed, Feed

    belongs_to :user, Orcasite.Accounts.User do
      api Orcasite.Accounts
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

  actions do
    defaults [:destroy]

    read :index do
      pagination do
        offset? true
        countable true
        default_limit 100
      end

      prepare build(load: [:uuid], sort: [inserted_at: :desc])
    end

    read :read do
      primary? true
      prepare build(load: [:uuid], sort: [inserted_at: :desc])
    end

    read :by_category do
      pagination do
        offset? true
        countable true
        default_limit 100
      end

      argument :category, :atom do
        allow_nil? false
        constraints one_of: Category.list()
      end

      prepare build(load: [:uuid], sort: [inserted_at: :desc])

      filter expr(category == ^arg(:category))
    end

    update :update do
      primary? true
      argument :candidate, :map

      change manage_relationship(:candidate, type: :append)
    end

    update :set_visible do
      accept [:visible]
      argument :visible, :boolean, default: true

      change set_attribute(:visible, arg(:visible))

      change fn changeset, _ ->
        changeset
        |> Ash.Changeset.after_action(fn changeset, detection ->
          candidate =
            detection
            |> Orcasite.Radio.load!(candidate: [:detections])
            |> Map.get(:candidate)

          # If all detections are hidden, make the candidate hidden
          candidate
          |> Ash.Changeset.for_update(:update, %{
            visible: !Enum.all?(candidate.detections, &(!&1.visible))
          })
          |> Orcasite.Radio.update!()

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
        :category,
        :send_notifications
      ]

      argument :feed_id, :string, allow_nil?: false

      argument :playlist_timestamp, :integer, allow_nil?: false
      argument :player_offset, :decimal, allow_nil?: false
      argument :listener_count, :integer, allow_nil?: true
      argument :description, :string

      argument :category, :atom,
        allow_nil?: false,
        constraints: [one_of: Category.list()]

      argument :send_notifications, :boolean, default: true

      change set_attribute(:playlist_timestamp, arg(:playlist_timestamp))
      change set_attribute(:player_offset, arg(:player_offset))
      change set_attribute(:listener_count, arg(:listener_count))
      change set_attribute(:description, arg(:description))
      change set_attribute(:category, arg(:category))
      change set_attribute(:source_ip, context(:actor_ip))

      change manage_relationship(:feed_id, :feed, type: :append)

      change fn changeset, %{actor: actor} ->
        case actor do
          %Orcasite.Accounts.User{} ->
            changeset
            |> Ash.Changeset.manage_relationship(:user, actor, type: :append)

          _ ->
            changeset
        end
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
            |> Orcasite.Radio.read!()
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
                |> Orcasite.Radio.create!()

              [candidate] ->
                candidate
                |> Ash.Changeset.for_update(:update, %{
                  detection_count: candidate.detection_count + 1,
                  min_time: datetime_min(candidate.min_time, detection.timestamp),
                  max_time: datetime_max(candidate.max_time, detection.timestamp)
                })
                |> Orcasite.Radio.update!()
            end

          detection
          |> Ash.Changeset.for_update(:update, %{candidate: candidate})
          |> Orcasite.Radio.update()
        end)
        |> Ash.Changeset.after_action(fn changeset, detection ->
          # Happens second
          detection =
            detection
            |> Orcasite.Radio.load!([:feed, :candidate])

          if Ash.Changeset.get_argument(changeset, :send_notifications) do
            Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
              Orcasite.Notifications.Notification.notify_new_detection(
                detection.id,
                detection.feed.slug,
                detection.description,
                detection.listener_count,
                detection.candidate.id
              )
            end)
          end

          {:ok, detection}
        end)
      end
    end
  end

  code_interface do
    define_for Orcasite.Radio

    define :submit_detection
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

  graphql do
    type :detection
    # Remove user until we want to make use of this behind
    # an authenticated/authorized call
    hide_fields [:user]

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
