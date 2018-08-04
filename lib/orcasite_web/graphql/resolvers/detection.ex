defmodule OrcasiteWeb.Resolvers.Detection do
  alias Orcasite.Radio

  def submit(
        %{feed_id: _feed_id, playlist_timestamp: _playlist_timestamp, time: _time} = detection_attrs,
        %{context: %{remote_ip: remote_ip}}
      ) do
    # Store new detection
    source_ip =
      remote_ip
      |> :inet_parse.ntoa()
      |> to_string()

    detection_attrs
    |> Map.put(:source_ip, source_ip)
    |> Radio.create_detection()
    |> case do
      {:ok, detection} -> {:ok, detection}
      {:error, _} -> {:error, :invalid}
    end
  end

  def submit(_, _), do: {:error, :invalid}
end
