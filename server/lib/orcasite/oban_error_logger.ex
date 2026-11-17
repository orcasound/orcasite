defmodule Orcasite.ObanErrorLogger do
  @moduledoc """
  Logs every failed Oban job attempt.

  Without this, a job that returns an error leaves nothing in the logs: Oban
  records the error on the job row and retries, but says nothing. Feed segment
  ingestion failed this way for a month unnoticed (#1092).

  Oban's own `attach_default_logger/1` also logs every job's start and stop,
  which is too much for the feeds queue, so this handles only failures.
  """
  require Logger

  @handler_id "orcasite-oban-error-logger"

  def attach do
    :telemetry.attach(@handler_id, [:oban, :job, :exception], &__MODULE__.handle_event/4, nil)
  end

  def handle_event([:oban, :job, :exception], _measurements, meta, _config) do
    %{job: job, kind: kind, reason: reason} = meta

    Logger.error(
      "Oban job failed: #{job.worker} id=#{job.id} queue=#{job.queue} " <>
        "attempt=#{job.attempt}/#{job.max_attempts} ids=#{inspect(id_args(job.args))} " <>
        "#{kind}: #{inspect(reason, limit: 20, printable_limit: 500)}"
    )
  rescue
    # :telemetry detaches a handler for good the first time it raises.
    error -> Logger.error("Orcasite.ObanErrorLogger failed: #{inspect(error)}")
  end

  # Only record ids are logged, because other arguments can hold personal
  # data such as an email address.
  defp id_args(args) when is_map(args) do
    Map.filter(args, fn {key, _value} ->
      key = to_string(key)
      key == "id" or String.ends_with?(key, "_id")
    end)
  end

  defp id_args(_args), do: %{}
end
