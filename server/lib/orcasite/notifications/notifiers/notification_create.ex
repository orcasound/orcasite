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
    IO.inspect(notification,
      label:
        "notification (server/lib/orcasite/notifications/notifiers/notification_create.ex:#{__ENV__.line})"
    )

    # Queue up an async process that:
      # Query Subscriptions that match this notification's event_type
      # and queue up a SubscriptionNotification create for each subscription as
      # long as the subscription has not had a recent SubscriptionNotification of the
      # same event type (or perhaps for the same subscriber)
  end
end
