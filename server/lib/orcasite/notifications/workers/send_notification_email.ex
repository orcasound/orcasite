defmodule Orcasite.Notifications.Workers.SendNotificationEmail do
  use Oban.Worker, queue: :email

  alias Orcasite.Notifications
  alias Orcasite.Notifications.SubscriptionNotification

  @impl Oban.Worker
  def perform(%Oban.Job{
        args: %{"subscription_notification_id" => subscription_notification_id} = _args
      }) do
    IO.inspect(subscription_notification_id, label: "subscription_notification_id")

    {:ok, sub_notif} =
      SubscriptionNotification
      |> Ash.Query.for_read(:read, %{id: subscription_notification_id})
      |> Notifications.read!()
      |> Notifications.load([:notification, :subscription, :subscriber])

    IO.inspect(sub_notif, label: "sub_notif (server/lib/orcasite/notifications/workers/send_notification_email.ex:#{__ENV__.line})")

    # Pull (with lock?) subscription, subscription notification, subscription, and subscriber
    # TODO: Send email

    Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
      sub_notif
      |> Ash.Changeset.for_update(:update, %{status: :sent, processed_at: DateTime.utc_now()})
      |> Notifications.update!()
    end)

    :ok
  end
end
