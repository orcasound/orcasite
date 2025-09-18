defmodule Orcasite.Notifications.Notification.Validations.RecentDetection do
  use Ash.Resource.Validation

  @impl Ash.Resource.Validation
  def validate(change, _opts, _context) do
    change
    |> Ash.Changeset.get_argument(:detection)
    |> Map.get(:timestamp)
    |> DateTime.compare(
      DateTime.utc_now()
      |> DateTime.add(-1, :hour)
    )
    |> case do
      :gt -> :ok
      :eq -> :ok
      _ -> {:error, "detection must be within the last hour"}
    end
  end
end
