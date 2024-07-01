defmodule Orcasite.Radio.FeedStreamQueue do
  use Broadway

  def start_link(_opts) do
    Broadway.start_link(__MODULE__,
      name: __MODULE__,
      producer: [
        module:
          {BroadwaySQS.Producer,
           queue_url: Application.get_env(:orcasite, :feed_stream_queue_url)}
      ],
      processors: [
        default: []
      ],
      batchers: [
        default: [
          batch_size: 10,
          batch_timeout: 2000
        ]
      ]
    )
  end

  @impl Broadway
  def handle_message(_, message, _), do: message

  @impl Broadway
  def handle_batch(_batcher, messages, _batch_info, _context) do
    paths =
      messages
      |> Enum.flat_map(fn e ->
        e.data
        |> Jason.decode()
        |> case do
          {:ok, %{"Records" => records}} ->
            records

          # Sometimes events are formatted as SNS messages
          {:ok, %{"Type" => "Notification", "Message" => message}} ->
            Jason.decode(message)
            |> case do
              {:ok, %{"Records" => records}} -> records
              _ -> []
            end

          _ ->
            []
        end
      end)
      |> Enum.flat_map(fn %{"s3" => %{"object" => %{"key" => object_path}}} ->
        if String.ends_with?(object_path, ".m3u8") do
          [%{m3u8_path: object_path}]
        else
          []
        end
      end)
      |> Enum.uniq()

    Task.Supervisor.start_child(Orcasite.TaskSupervisor, fn ->
      paths
      |> Enum.map(&Map.merge(&1, %{update_segments?: true, link_streams?: true}))
      |> Ash.bulk_create(
        Orcasite.Radio.FeedStream,
        :from_m3u8_path,
        return_errors?: true,
        stop_on_error?: true,
        upsert?: true,
        upsert_identity: :feed_stream_timestamp
      )
    end)

    messages
  end
end
