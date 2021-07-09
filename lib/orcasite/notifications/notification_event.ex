defmodule Orcasite.Notifications.NotificationEvents do
    use Ecto.Schema
    import Ecto.Changeset
    
    schema "notification_events" do
      field(:candidate_id, :integer)
      field(:notified_at, :utc_datetime)
      field(:notified_by, :string)
      
      timestamps()
    end
  
    def changeset(notification_events, attrs) do
      notification_events
      |> cast(attrs, [:candidate_id, :notified_at, :notified_by])
      |> validate_required([:candidate_id])
    end
    
end
    