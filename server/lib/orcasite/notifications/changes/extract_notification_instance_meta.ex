defmodule Orcasite.Notifications.Changes.ExtractNotificationInstanceMeta do
  use Ash.Resource.Change

  alias Orcasite.Notifications
  alias Orcasite.Notifications.{Notification, Subscription}

  def change(
        %{arguments: %{notification: notification_id, subscription: subscription_id}} = changeset,
        _opts,
        _context
      )
      when is_binary(notification_id) and is_binary(subscription_id) do
    # Get relevant details for this notification instance:
    # For either event type (right now), get the node, timestamp, maybe
    # construct the link to the node

    # Based on the channel, do:
    # If it's an email, get the email address, set subject and
    # body based on event type

    with {:get_sub, {:ok, subscription}} <-
           {:get_sub, Notifications.get(Subscription, changeset.arguments.subscription)},
         {:get_notif, {:ok, notification}} <-
           {:get_notif, Notifications.get(Notification, changeset.arguments.notification)} do
      changeset
      |> Ash.Changeset.change_attribute(:meta, %{
        subscriber_name: Map.get(subscription.meta, "name"),
        channel: Map.get(subscription.meta, "channel"),
        event_type: notification.event_type,
        node: Map.get(notification.meta, "node"),
        timestamp: Map.get(notification.meta, "timestamp")
      })
      |> Ash.Changeset.change_attribute(:channel, Map.get(subscription.meta, "channel"))
    else
      {:get_sub, {:error, _}} ->
        changeset
        |> Ash.Changeset.add_error("Subscription not found")

      {:get_notif, {:error, _}} ->
        changeset
        |> Ash.Changeset.add_error("Notification not found")
    end
  end

  def change(changeset, _opts, _context) do
    changeset
  end
end
