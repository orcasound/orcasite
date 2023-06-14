defmodule Orcasite.Notifications.Notifiers.NotifySubscriptions do
  use Ash.Notifier
  require Logger

  def notify(
        %Ash.Notifier.Notification{
          resource: Orcasite.Notifications.Notification,
          action: %{type: :create},
          actor: _actor,
          data: notification
        } = _notif
      ) do
    # Queue up an async process that:
    # Query Subscriptions that match this notification's event_type
    # and queue up a NotificationInstance create for each subscription as
    # long as the subscription has not had a recent NotificationInstance of the
    # same event type (or perhaps for the same subscriber)
    Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
      Orcasite.Notifications.Subscription
      |> Ash.Query.for_read(:available_for_notification, %{
        notification_id: notification.id,
        event_type: notification.event_type
      })
      |> Orcasite.Notifications.stream!()
      |> Stream.map(fn subscription ->
        Orcasite.Notifications.NotificationInstance
        |> Ash.Changeset.for_create(:create_with_relationships, %{
          notification: notification.id,
          subscription: subscription.id
        })
        |> Orcasite.Notifications.create!()
      end)
      |> Stream.run()
    end)
    :ok
  end
end
