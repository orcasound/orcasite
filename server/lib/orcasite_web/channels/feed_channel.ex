defmodule OrcasiteWeb.FeedChannel do
  use OrcasiteWeb, :channel
  alias OrcasiteWeb.Presence

  require Logger

  def join("feed:" <> feed_slug, _params, socket) do
    Process.send_after(self(), :after_join, 100)
    {:ok, socket |> assign(:feed_slug, feed_slug)}
  end

  def handle_info({:join, _presences}, socket) do
    publish_listener_counts(socket)
    {:noreply, socket}
  end

  def handle_info({:leave, _presences}, socket) do
    publish_listener_counts(socket)

    Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
      Process.sleep(1000)
      publish_listener_counts(socket)
    end)

    {:noreply, socket}
  end

  def handle_info(:after_join, socket) do
    {:ok, _} =
      Presence.track(socket, socket.assigns.feed_slug, %{
        feed: socket.assigns.feed_slug,
        online_at: inspect(System.system_time(:second))
      })

    presence_list = Presence.list(socket)

    publish_listener_counts(
      {socket.assigns.feed_slug, listener_count(socket.topic, presence_list)}
    )

    push(socket, "presence_state", presence_list)
    {:noreply, socket}
  end

  def handle_info(msg, socket) do
    Logger.warning("Unknown handle_info message for FeedChannel: #{inspect(msg)}")
    {:noreply, socket}
  end

  def terminate(reason, socket) do
    publish_listener_counts(socket)

    Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
      Process.sleep(1000)
      publish_listener_counts(socket)
    end)

    reason
  end

  def listener_count("feed:" <> feed_slug = _topic, presences) do
    presences
    |> Map.get(feed_slug, %{})
    |> Map.get(:count, 0)
  end

  def publish_listener_counts({feed_slug, count}) do
    key = "listener_counts:" <> feed_slug

    Orcasite.Cache.put(key, count)

    Phoenix.PubSub.broadcast(Orcasite.PubSub, key, :update_counts)
    {key, count}
  end

  def publish_listener_counts(socket) do
    counts = listener_count(socket.topic, Presence.list(socket))
    publish_listener_counts({socket.assigns.feed_slug, counts})
  end
end
