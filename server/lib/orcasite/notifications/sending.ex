defmodule Orcasite.Notifications.Sending do
  alias Orcasite.Repo
  alias Orcasite.Notifications.{
    SubscriptionNotification,
    Workers
  }

  def queue(%SubscriptionNotification{channel: :email} = sub_notif) do
    {:ok, %{id: _job_id}} =
      %{subscription_notification: sub_notif}
      |> Workers.SendNotificationEmail.new()
      |> Repo.insert()

    # notification
    # |> Ash.Changeset.for_update(:update, %{status: :pending, meta: %{job_id: job_id}})
    # |> Notifications.update!()
  end
end
