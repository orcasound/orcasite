defmodule Orcasite.Notifications.Sending do
  alias Orcasite.Repo
  alias Orcasite.Notifications.{
    SubscriptionNotification,
    Workers
  }

  def queue(%SubscriptionNotification{id: id, channel: :email} = _sub_notif) do
    {:ok, %{id: _job_id}} =
      %{subscription_notification_id: id}
      |> Workers.SendNotificationEmail.new()
      |> Repo.insert()

    # notification
    # |> Ash.Changeset.for_update(:update, %{status: :pending, meta: %{job_id: job_id}})
    # |> Notifications.update!()
  end

  def queue(%SubscriptionNotification{id: id, channel: :newsletter} = _sub_notif) do
    {:ok, %{id: _job_id}} =
      %{subscription_notification_id: id}
      |> Workers.SendNotificationNewsletter.new()
      |> Repo.insert()

    # notification
    # |> Ash.Changeset.for_update(:update, %{status: :pending, meta: %{job_id: job_id}})
    # |> Notifications.update!()
  end
end
