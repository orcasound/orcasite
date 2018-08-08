defmodule OrcasiteWeb.Resolvers.Detection do
  alias Orcasite.Radio

  def submit(
        %{
          feed_id: feed_id,
          playlist_timestamp: _playlist_timestamp,
          player_offset: _player_offset
        } = detection_attrs,
        %{context: %{remote_ip: remote_ip}}
      ) do
    # Store new detection
    source_ip =
      remote_ip
      |> :inet_parse.ntoa()
      |> to_string()

    :timer.sleep(3000)

    # with :ok <- Radio.verify_can_submit_detection(feed_id, source_ip, 10) do
    #   detection_attrs
    #   |> Map.put(:source_ip, source_ip)
    #   |> Radio.create_detection()
    #   |> case do
    #     {:ok, detection} -> {:ok, detection}
    #     {:error, _} -> {:error, :invalid}
    #   end
    # else
    #   error -> error
    # end
    {:error, :nyi}
  end

  def submit(_, _), do: {:error, :invalid}
end
