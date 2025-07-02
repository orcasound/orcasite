defmodule Orcasite.Radio.Detection.Changes.GenerateSpectrograms do
  use Ash.Resource.Change

  @impl Ash.Resource.Change
  def change(changeset, _opts, _context) do
    changeset
    |> Ash.Changeset.after_action(fn _change, detection ->
      Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
        feed = detection |> Ash.load!(:feed) |> Map.get(:feed)

        # minutes
        buffer = 5
        now = DateTime.utc_now()

        start_time = detection.timestamp |> DateTime.add(-buffer, :minute)

        plus_buffer =
          detection.timestamp
          |> DateTime.add(buffer, :minute)

        end_time =
          plus_buffer
          |> DateTime.compare(now)
          |> case do
            :gt -> now
            _ -> plus_buffer
          end

        feed
        |> Ash.Changeset.for_update(:generate_spectrogram, %{
          start_time: start_time,
          end_time: end_time
        })
        |> Ash.update(authorize?: false)
      end)

      {:ok, detection}
    end)
  end
end
