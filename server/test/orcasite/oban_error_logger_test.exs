defmodule Orcasite.ObanErrorLoggerTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureLog

  alias Orcasite.ObanErrorLogger

  @job %Oban.Job{
    id: 7,
    worker: "Orcasite.Radio.Workers.UpdateFeedSegments",
    queue: "feeds",
    attempt: 2,
    max_attempts: 20,
    args: %{"feed_stream_id" => "fdstrm_1", "email" => "someone@example.com"}
  }

  test "logs the failed job with its ids and reason, without other arguments" do
    log =
      capture_log(fn ->
        ObanErrorLogger.handle_event(
          [:oban, :job, :exception],
          %{},
          %{job: @job, kind: :error, reason: {:rolling_playlist, 12}},
          nil
        )
      end)

    assert log =~ "Oban job failed: Orcasite.Radio.Workers.UpdateFeedSegments id=7"
    assert log =~ "attempt=2/20"
    assert log =~ "fdstrm_1"
    assert log =~ "rolling_playlist"
    refute log =~ "someone@example.com"
  end

  test "does not raise on metadata it does not expect" do
    log =
      capture_log(fn ->
        assert ObanErrorLogger.handle_event([:oban, :job, :exception], %{}, %{}, nil) == :ok
      end)

    assert log =~ "Orcasite.ObanErrorLogger failed"
  end
end
