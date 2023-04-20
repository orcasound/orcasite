defmodule Orcasite.Notifications.Sending do
  alias Orcasite.Notifications.{Notification, Subscriber, Subscription, SubscriptionNotification}

  def send(%SubscriptionNotification{status: :unsent, id: id} = notification) do
    Task.Supvervisor.start_child(Orcasite.TaskSupervisor, fn ->
      do_send(notification)
    end)
  end

  def do_send(%SubscriptionNotification{status: :unsent, id: id} = notification) do
    # Pull notification, subscription, and subscriber
    # Delegate sending to the appropriate module based on subscription channel
  end
end
