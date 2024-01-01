defmodule Orcasite.Notifications.Workers.SendNotificationEmail do
  use Oban.Worker, queue: :email, unique: [keys: [:notification_id, :subscription_id]]

  alias Orcasite.Notifications
  alias Orcasite.Notifications.{Subscription, NotificationInstance, Notification}

  @impl Oban.Worker
  def perform(%Oban.Job{
        args:
          %{
            "notification_id" => notification_id,
            "subscription_id" => subscription_id,
            "meta" => meta,
            "notification_instance_id" => notification_instance_id
          } = _args
      }) do
    notification = Notification |> Notifications.get!(notification_id)

    subscription =
      Subscription |> Notifications.get!(subscription_id) |> Notifications.load!(:subscriber)

    params =
      [meta, subscription.meta, notification.meta]
      |> Enum.map(&stringify_map/1)
      |> Enum.reduce(&Map.merge/2)

    unsubscribe_token =
      Orcasite.Notifications.Subscription.unsubscribe_token(subscription)

    notifications_since =
      subscription.last_notification_id
      |> case do
        nil ->
          []

        notif_id ->
          Notification
          |> Ash.Query.for_read(:since_notification, %{notification_id: notif_id})
          |> Orcasite.Notifications.read!()
      end
      |> Enum.filter(&(&1.id != notification_id))

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

    subscription
    |> Subscription.update_last_notification(notification_id)

    Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
      NotificationInstance
      |> Notifications.get!(notification_instance_id)
      |> case do
        nil ->
          nil

        notif_instance ->
          notif_instance
          |> Ash.Changeset.for_destroy(:destroy)
          |> Notifications.destroy!()
      end
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
      {:allow, _count} ->
        :ok

      {:deny, _limit} ->
        Process.sleep(250)
        continue?()
    end
  end
end
