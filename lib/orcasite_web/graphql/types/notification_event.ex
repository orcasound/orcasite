defmodule OrcasiteWeb.Types.NotificationEvent do

    use Absinthe.Schema.Notation

    object :notification_event do
        field(:candidate_id, :id)
        field(:notified_by, :string)
        field(:notified_at, :datetime)
    end

end