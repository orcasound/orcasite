defmodule Orcasite.Notifications.NotificationEvents do
    use Ecto.Schema
    import Ecto.Changeset
    
    alias Orcasite.Radio.Candidate
    
    schema "notification_events" do
      field(:notified_by, :string)
      
      belongs_to(:candidates, Candidate, references: :candidate_id, foreign_key: :candidate_id)

      timestamps()
    end
  
    def changeset(notification_events, attrs) do
      notification_events
      |> cast(attrs, [:candidate_id, :notified_by])
      |> validate_required([:candidate_id])
    end
    
end
    