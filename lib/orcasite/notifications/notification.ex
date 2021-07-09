defmodule Orcasite.Notifications do
  import Ecto.Query, warn: false

  alias Orcasite.Repo
  alias Orcasite.Notifications.NotificationEvents

  def create_notification_event(attrs \\ %{}) do
    %NotificationEvents{}
    |> NotificationEvents.changeset(attrs)
    |> Repo.insert()
  end

  def list_notification_event do
    Repo.all(NotificationEvents)
  end
end
  