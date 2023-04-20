defmodule Orcasite.Notifications.Workers.Email do
  use Oban.Worker, queue: :email

  @impl Oban.Worker
  def perform(%Oban.Job{
        args: %{"subscription_notification_id" => subscription_notification_id} = args
      }) do
    # Pull subscription, subscription notification, subscription, and subscriber
    IO.inspect(subscription_notification_id, label: "subscription_notification_id")

    :ok
  end
end
