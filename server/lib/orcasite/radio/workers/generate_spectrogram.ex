defmodule Orcasite.Radio.Workers.GenerateSpectrogram do
  @max_attempts 3

  use Oban.Worker,
    queue: :audio_images,
    unique: [
      keys: [:audio_image_id],
      period: :infinity,
      states: [:available, :scheduled, :executing]
    ],
    max_attempts: @max_attempts

  @impl Oban.Worker
  def perform(%Oban.Job{args: %{"audio_image_id" => audio_image_id}, attempt: attempt}) do
    # 900/min equivalent in a second
    :ok = Orcasite.RateLimiter.continue?(:generate_spectrogram, 1_000, 15)

    audio_image = Orcasite.Radio.AudioImage |> Ash.get!(audio_image_id)

    audio_image
    |> Ash.Changeset.for_update(:generate_spectrogram)
    |> Ash.update(authorize?: false, timeout: :timer.minutes(3))
    |> case do
      {:ok, %{status: :complete}} -> :ok
      {:ok, %{last_error: error}} -> {:error, {:not_complete, error}}
      {:error, err} -> {:error, err}
    end
    |> case do
      :ok ->
        :ok

      error ->
        if attempt >= @max_attempts, do: set_failed(audio_image)
        error
    end
  catch
    # A crash (rather than a returned error) used to leave the image
    # `processing` for ever once Oban gave up on the job; the page then shows
    # a placeholder that never resolves. Exits count too: a missing AWS
    # credential surfaces as ExAws's credential cache crashing under us.
    kind, reason ->
      if attempt >= @max_attempts do
        Orcasite.Radio.AudioImage
        |> Ash.get(audio_image_id, authorize?: false)
        |> case do
          {:ok, audio_image} -> set_failed(audio_image)
          _ -> :ok
        end
      end

      :erlang.raise(kind, reason, __STACKTRACE__)
  end

  defp set_failed(audio_image) do
    audio_image
    |> Ash.reload!()
    |> Ash.Changeset.for_update(:set_failed)
    |> Ash.update(authorize?: false)
  end

  @impl Oban.Worker
  @doc """
  Takes max 1 minute (for 10s of audio), add 50%. The 1 minute is due to lambda cold start.
  """
  def timeout(_job), do: :timer.seconds(90)
end
