defmodule Orcasite.Notifications.Sending do
  alias Orcasite.Notifications.{
    NotificationInstance,
    Workers
  }

  def queue(
        %NotificationInstance{
          channel: :email,
          id: id,
          notification_id: notification_id,
          subscription_id: subscription_id
        } = _notification_instance
      ) do
    {:ok, %{id: _job_id}} =
      %{
        notification_instance_id: id,
        notification_id: notification_id,
        subscription_id: subscription_id
      }
      |> Workers.SendNotificationEmail.new()
      |> Oban.insert()
  end
end
