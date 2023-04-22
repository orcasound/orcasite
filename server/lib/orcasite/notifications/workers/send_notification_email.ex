defmodule Orcasite.Notifications.Workers.SendNotificationEmail do
  use Oban.Worker, queue: :email

  alias Orcasite.Notifications
  alias Orcasite.Notifications.SubscriptionNotification

  @impl Oban.Worker
  def perform(%Oban.Job{
        args: %{"subscription_notification_id" => subscription_notification_id} = _args
      }) do
    {:ok, [sub_notif]} =
      SubscriptionNotification
      |> Ash.Query.for_read(:read, %{id: subscription_notification_id})
      |> Notifications.read!()
      |> Notifications.load([:notification, subscription: [:subscriber]])

    %{
      to: sub_notif.meta["email"],
      name: sub_notif.meta["subscriber_name"],
      node: sub_notif.meta["node"]
    }
    |> Orcasite.Notifications.Email.new_detection_email()
    |> Orcasite.Mailer.deliver()

    Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
      sub_notif
      |> Ash.Changeset.for_update(:update, %{status: :sent, processed_at: DateTime.utc_now()})
      |> Notifications.update!()
    end)

    :ok
  end
end
