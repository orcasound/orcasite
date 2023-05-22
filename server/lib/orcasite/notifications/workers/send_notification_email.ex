defmodule Orcasite.Notifications.Workers.SendNotificationEmail do
  use Oban.Worker, queue: :email, unique: [keys: [:notification_id, :subscription_id]]

  alias Orcasite.Notifications
  alias Orcasite.Notifications.SubscriptionNotification

  @impl Oban.Worker
  def perform(%Oban.Job{
        args: %{"subscription_notification_id" => subscription_notification_id} = _args
      }) do
    sub_notif =
      SubscriptionNotification
      |> Ash.Query.for_read(:read, %{id: subscription_notification_id})
      |> Notifications.read!()
      |> Notifications.load([:notification, subscription: [:subscriber]])
      |> case do
        {:ok, notifs} when is_list(notifs) ->
          notifs |> List.last()

        {:ok, notif} ->
          notif
      end

    params =
      sub_notif.meta
      |> stringify_map()
      |> Map.merge(stringify_map(sub_notif.subscription.meta))
      |> Map.merge(stringify_map(sub_notif.notification.meta))

    %{
      to: params["email"],
      name: params["subscriber_name"],
      node: params["node"]
    }
    |> email_for_notif(stringify(params["event_type"]))
    |> Orcasite.Mailer.deliver()

    sub_notif.subscription
    |> Ash.Changeset.for_update(:update, %{
      last_notified_at: DateTime.utc_now()
    })
    |> Ash.Changeset.manage_relationship(:last_notification, sub_notif.notification, type: :append)
    |> Notifications.update!()

    Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
      sub_notif
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
