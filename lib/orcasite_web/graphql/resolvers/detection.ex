defmodule OrcasiteWeb.Resolvers.Detection do
  alias Orcasite.Radio
  alias OrcasiteWeb.Paginated

  def index(_, %{context: %{current_user: %{admin: true}}}) do
    {:ok, Radio.list_detections()}
  end

  def index(_, _) do
    IO.puts("in index() in detection.ex")
    {:error, :not_authorized}
  end

  def list_candidates(args, %{context: %{current_user: %{admin: true}}}) do
    {:ok, Paginated.format(Radio.list_candidates(args))}
  end

  def list_candidates(_,_) do
    IO.puts("...candidates..")
    {:ok, Paginated.format(Radio.list_candidates())}
  end

  def list_detections(_,_) do
    IO.puts("...list_detections...")
    {:ok, Paginated.format(Radio.list_detections())}
  end

  def list_detections_for_feed(params, _) do
    IO.puts("...list_detections for feed...")
    IO.puts(Radio.get_feed_by_slug(params.slug).id)
    IO.puts("hahaha")
    {:ok, Paginated.format(Radio.list_detections_for_feed(Radio.get_feed_by_slug(params.slug).id))}
  end

  #def list_candidates(_, _), do
  #  {:error, :not_authorized}
  #end

  def create(
        %{
          feed_id: feed_id,
          playlist_timestamp: _playlist_timestamp,
          player_offset: _player_offset,
          description: _description,
          type: _type
        } = detection_attrs,
        %{context: %{remote_ip: remote_ip}}
      ) do
    IO.puts("Got here....")
    # Store new detection
    source_ip =
      remote_ip
      |> :inet_parse.ntoa()
      |> to_string()

    # TODO: create detection with the given type and add that to the database (instead of using the default type)
    with :ok <- Radio.verify_can_submit_detection(feed_id, source_ip, lockout_seconds()) do
      detection_attrs
      |> Map.put(:source_ip, source_ip)
      |> Radio.create_detection_with_candidate() # formerly create_detection_with_candidate()
      |> case do
        {:ok, detection} ->
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
