defmodule Orcasite.Radio.Workers.GenerateSpectrogram do
  use Oban.Worker,
    queue: :audio_images,
    unique: [
      keys: [:audio_image_id],
      period: :infinity,
      states: [:available, :scheduled, :executing]
    ],
    max_attempts: 3

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
        if attempt >= 3 do
          audio_image
          |> Ash.reload!()
          |> Ash.Changeset.for_update(:set_failed)
          |> Ash.update(authorize?: false)
        end

        error
    end
  end
end
