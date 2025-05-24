defmodule Orcasite.Notifications.Workers.SendNotificationEmail do
  use Oban.Worker, queue: :email, unique: [keys: [:notification_id, :subscription_id]]

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
    notification = Notification |> Ash.get!(notification_id, authorize?: false)

    subscription =
      Subscription |> Ash.get!(subscription_id) |> Ash.load!(:subscriber)

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
          |> Ash.read!(authorize?: false)
      end
      |> Enum.filter(&(&1.id != notification_id))

    :ok = Orcasite.RateLimiter.continue?("ses_email", 1_000, 14)

    %{meta: params}
    |> Map.merge(%{
      to: params["email"],
      name: params["subscriber_name"],
      node: params["node"] || "",
      unsubscribe_token: unsubscribe_token,
      notifications_since: notifications_since |> Enum.map(& &1.meta),
      notifications_since_count: Enum.count(notifications_since)
    })
    |> email_for_notif(stringify(notification.event_type))
    |> Orcasite.Mailer.deliver()

    subscription
    |> Subscription.update_last_notification(notification_id, authorize?: false)

    Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
      NotificationInstance
      |> Ash.get(notification_instance_id)
      |> case do
        {:error, _} ->
          nil

        {:ok, notif_instance} ->
          notif_instance
          |> Ash.Changeset.for_destroy(:destroy)
          |> Ash.destroy!()
      end
    end)

    Orcasite.Notifications.Notification.increment_notified_count(notification, authorize?: false)

    :ok
  end

  defp email_for_notif(params, "new_detection"),
    do: Orcasite.Notifications.Email.new_detection_email(params)

  defp email_for_notif(params, "confirmed_candidate"),
    do: Orcasite.Notifications.Email.confirmed_candidate_email(params)

  defp email_for_notif(params, "live_bout"),
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
end
