defmodule OrcasiteWeb.Resolvers.Detection do
  alias Orcasite.Radio
  alias OrcasiteWeb.Paginated

  def index(_, _) do
    {:ok, Radio.list_all_detections()}
  end

  def list_candidates(args, _) do
    {:ok, Paginated.format(Radio.list_candidates(args))}
  end

  def list_detections(args, _) do
    {:ok, Paginated.format(Radio.list_detections(args))}
  end

  def create(
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

    with :ok <- Radio.verify_can_submit_detection(feed_id, source_ip, lockout_seconds()) do
      detection_attrs
      |> Map.put(:source_ip, source_ip)
      |> Radio.create_detection_with_candidate()
      |> case do
        {:ok, detection} ->
          # Send notification for new detection
          Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
            %{slug: node} = Radio.get_feed!(feed_id)
            Orcasite.Notifications.Notification.notify_new_detection(detection.id, node)
          end)

          {:ok,
           %{
             detection: detection,
             lockout_initial: lockout_seconds(),
             lockout_remaining: lockout_seconds()
           }}

        {:error, _} ->
          {:error, :invalid}
      end
    else
      {:error, %{lockout_remaining: lockout_remaining}} ->
        {:error,
         %{
           message: "lockout",
           details: %{lockout_remaining: lockout_remaining, lockout_initial: lockout_seconds()}
         }}

      error ->
        error
    end
  end

  def submit(_, _), do: {:error, :invalid}

  # TODO: Define this as an env var
  defp lockout_seconds(), do: 10
end
