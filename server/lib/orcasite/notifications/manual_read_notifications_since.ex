defmodule Orcasite.Notifications.ManualReadNotificationsSince do
  use Ash.Resource.ManualRead

  alias Orcasite.Notifications
  alias Orcasite.Notifications.Notification

  require Ash.Query

  def read(query, _ecto_query, _opts, _context) do
    # Get notifications of the same type after the passed-in notification_id
    notification_id =
      query
      |> Ash.Query.get_argument(:notification_id)

    notification =
      Notification
      |> Notifications.get!(notification_id)

    Notification
    |> Ash.Query.filter(
        event_type == ^notification.event_type and
        inserted_at > ^notification.inserted_at
    )
    |> Notifications.read()
  end
end
