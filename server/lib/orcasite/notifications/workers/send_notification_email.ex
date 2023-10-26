defmodule Orcasite.Notifications.Workers.SendNotificationEmail do
  use Oban.Worker, queue: :email, unique: [keys: [:notification_id, :subscription_id]]

  alias Orcasite.Notifications
  alias Orcasite.Notifications.{Subscription, NotificationInstance, Notification}

  @impl Oban.Worker
  def perform(%Oban.Job{
        args: %{"notification_instance_id" => notification_instance_id} = _args
      }) do
    notif_instance =
      NotificationInstance
      |> Notifications.get!(notification_instance_id)
      |> Notifications.load!([:notification, subscription: [:subscriber]])

    params =
      notif_instance.meta
      |> Map.merge(notif_instance.subscription.meta)
      |> Map.merge(notif_instance.notification.meta)
      |> stringify_map()

    unsubscribe_token =
      Orcasite.Notifications.Subscription.unsubscribe_token(notif_instance.subscription)

    notifications_since =
      notif_instance.subscription.last_notification_id
      |> case do
        nil ->
          []

        notif_id ->
          Notification
          |> Ash.Query.for_read(:since_notification, %{notification_id: notif_id})
          |> Orcasite.Notifications.read!()
      end
      |> Enum.filter(&(&1.id != notif_instance.notification_id))

    :ok = continue?()

    %{meta: params}
    |> Map.merge(%{
      to: params["email"],
      name: params["subscriber_name"],
      node: params["node"] || "",
      unsubscribe_token: unsubscribe_token,
      notifications_since: notifications_since |> Enum.map(& &1.meta),
      notifications_since_count: Enum.count(notifications_since)
    })
    |> email_for_notif(stringify(params["event_type"]))
    |> Orcasite.Mailer.deliver()

    notif_instance.subscription
    |> Subscription.update_last_notification(notif_instance.notification_id)

    Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
      notif_instance
      |> Ash.Changeset.for_destroy(:destroy)
      |> Notifications.destroy!()
    end)

    :ok
  end

  defp email_for_notif(params, "new_detection"),
    do: Orcasite.Notifications.Email.new_detection_email(params)

  defp email_for_notif(params, "confirmed_candidate"),
    do: Orcasite.Notifications.Email.confirmed_candidate_email(params)

  defp stringify(name) when is_atom(name), do: Atom.to_string(name)
  defp stringify(name), do: name

  defp stringify_map(map) do
    map
    |> Map.new(fn
      {k, v} when is_atom(k) ->
        {Atom.to_string(k), v}

      {k, v} ->
        {k, v}
    end)
  end

  def continue?() do
    case Hammer.check_rate("ses_email", 1_000, 14) do
      {:allow, _count} -> :ok
      {:deny, _limit} ->
        Process.sleep(250)
        continue?()
    end
  end
end
