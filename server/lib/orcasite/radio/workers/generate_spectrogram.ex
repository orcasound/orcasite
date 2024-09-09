defmodule Orcasite.Radio.Workers.GenerateSpectrogram do
  use Oban.Worker,
    queue: :feeds,
    unique: [
      keys: [:audio_image_id],
      period: :infinity,
      states: [:available, :scheduled, :executing]
    ],
    max_attempts: 3

  @impl Oban.Worker
  def perform(%Oban.Job{args: %{"audio_image_id" => audio_image_id}}) do
    Orcasite.Radio.AudioImage
    |> Ash.get!(audio_image_id)
    |> Ash.Changeset.for_update(:generate_spectrogram)
    |> Ash.update(authorize?: false)
  end
end
