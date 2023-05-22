defmodule Orcasite.Notifications.Sending do
  alias Orcasite.Notifications.{
    SubscriptionNotification,
    Workers
  }

  def queue(
        %SubscriptionNotification{
          channel: :email,
          id: id,
          notification_id: notification_id,
          subscription_id: subscription_id
        } = _subscription_notification
      ) do
    {:ok, %{id: _job_id}} =
      %{
        subscription_notification_id: id,
        notification_id: notification_id,
        subscription_id: subscription_id
      }
      |> Workers.SendNotificationEmail.new()
      |> Oban.insert()
  end
end
