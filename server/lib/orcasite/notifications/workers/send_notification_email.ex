defmodule Orcasite.Notifications.Workers.SendNotificationEmail do
  use Oban.Worker, queue: :email, unique: [keys: [:notification_id, :subscription_id]]

  alias Orcasite.Notifications
  alias Orcasite.Notifications.{Subscription, NotificationInstance}

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
      |> stringify_map()
      |> Map.merge(stringify_map(notif_instance.subscription.meta))
      |> Map.merge(stringify_map(notif_instance.notification.meta))

    unsubscribe_token =
      Orcasite.Notifications.Subscription.unsubscribe_token(notif_instance.subscription)

    :ok = Orcasite.RateLimiter.continue?(:ses, 1)
    %{
      to: params["email"],
      name: params["subscriber_name"],
      node: params["node"] || "",
      unsubscribe_token: unsubscribe_token
    }
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
end
