defmodule Orcasite.Notifications.Job do
  use Ash.Resource,
    extensions: [AshAdmin.Resource, AshGraphql.Resource],
    data_layer: AshPostgres.DataLayer

  @states [:available, :scheduled, :executing, :retryable, :completed, :discarded, :cancelled]

  postgres do
    table "oban_jobs"
    repo Orcasite.Repo
    migrate? false
  end

  attributes do
    integer_primary_key :id

    attribute :state, :atom do
      constraints one_of: @states

      writable? false
    end

    attribute :queue, :string, writable?: false
    attribute :worker, :string, writable?: false
    attribute :args, :map, writable?: false
    attribute :errors, {:array, :map}, writable?: false
    attribute :attempt, :integer, writable?: false
    attribute :max_attempts, :integer, writable?: false

    attribute :inserted_at, :utc_datetime, writable?: false
    attribute :scheduled_at, :utc_datetime, writable?: false
    attribute :attempted_at, :utc_datetime, writable?: false
    attribute :completed_at, :utc_datetime, writable?: false
    attribute :cancelled_at, :utc_datetime, writable?: false
    attribute :discarded_at, :utc_datetime, writable?: false

    attribute :attempted_by, {:array, :string}, writable?: false
    attribute :priority, :integer, writable?: false
    attribute :tags, {:array, :string}, writable?: false
    attribute :meta, :map, writable?: false
  end

  actions do
    defaults [:read]

    read :index do
      pagination do
        offset? true
        countable true
        default_limit 100
      end

      argument :state, :atom do
        constraints one_of: @states
      end

      argument :notification_id, :string
      argument :subscription_id, :string
      argument :email, :string
      argument :node, :string

      argument :event_type, :atom do
        constraints one_of: Orcasite.Notifications.Event.types()
      end

      filter expr(
               if(is_nil(^arg(:notification_id)),
                 do: true,
                 else: args[:notification_id] == ^arg(:notification_id)
               ) and
                 if(is_nil(^arg(:subscription_id)),
                   do: true,
                   else: args[:subscription_id] == ^arg(:subscription_id)
                 ) and
                 if(is_nil(^arg(:email)),
                   do: true,
                   else: args[:meta][:email] == ^arg(:email)
                 ) and
                 if(is_nil(^arg(:node)),
                   do: true,
                   else: args[:meta][:node] == ^arg(:node)
                 ) and
                 if(is_nil(^arg(:event_type)),
                   do: true,
                   else: args[:meta][:event_type] == ^arg(:event_type)
                 ) and
                 if(is_nil(^arg(:state)),
                   do: true,
                   else: state == ^arg(:state)
                 )
             )
    end
  end

  admin do
    table_columns [
      :id,
      :state,
      :args,
      :attempt,
      :max_attempts,
      :inserted_at,
      :scheduled_at,
      :completed_at,
      :tags
    ]

    format_fields args: {__MODULE__, :fetch_meta, []},
                  meta: {Jason, :encode!, []},
                  inserted_at: {Calendar, :strftime, ["%m/%d %H:%M:%S %Z"]},
                  scheduled_at: {Calendar, :strftime, ["%m/%d %H:%M:%S %Z"]},
                  completed_at: {Calendar, :strftime, ["%m/%d %H:%M:%S %Z"]}
  end

  preparations do
    prepare build(sort: [inserted_at: :desc])
  end

  def fetch_meta(args) do
    args
    |> Map.get("meta")
    |> Jason.encode!()
  end
end
