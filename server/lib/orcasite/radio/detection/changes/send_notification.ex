defmodule Orcasite.Radio.Detection.Changes.SendNotification do
  use Ash.Resource.Change

  @impl Ash.Resource.Change
  def change(changeset, _opts, _context) do
    Ash.Changeset.after_transaction(changeset, fn _change, detection ->
      detection = Ash.load!(detection, [:candidate, :feed])

      Orcasite.Notifications.Notification.notify_new_detection(
        detection,
        detection.candidate,
        detection.feed,
        authorize?: false
      )

      {:ok, detection}
    end)
  end
end
