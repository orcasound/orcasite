defmodule Orcasite.Notifications do
  import Ecto.Query, warn: false

  alias Orcasite.Repo
  alias Orcasite.Notifications.NotificationEvents

  def create_notification_event(attrs \\ %{}) do
    # Record current time in notified_at column
    timestamp = DateTime.utc_now()

    %NotificationEvents{}
    |> NotificationEvents.changeset(Map.merge(attrs, %{notified_at: timestamp}))
    |> Repo.insert()
  end

  def list_notification_event do
    Repo.all(NotificationEvents)
  end
end
  