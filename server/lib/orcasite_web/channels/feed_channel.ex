defmodule OrcasiteWeb.FeedChannel do
  use OrcasiteWeb, :channel
  alias OrcasiteWeb.Presence

  def join("feed:" <> feed_slug, _params, socket) do
    send(self(), :after_join)
    {:ok, socket |> assign(:feed_slug, feed_slug)}
  end

  def handle_info({:join, _presences}, socket) do
    publish_listener_counts(socket)
    {:noreply, socket}
  end

  def handle_info({:leave, _presences}, socket) do
    publish_listener_counts(socket)
    {:noreply, socket}
  end

  def handle_info(:after_join, socket) do
    {:ok, _} =
      Presence.track(socket, socket.assigns.feed_slug, %{
        feed: socket.assigns.feed_slug,
        online_at: inspect(System.system_time(:second))
      })

    presence_list = Presence.list(socket)
    publish_listener_counts({socket.assigns.feed_slug, listener_count(presence_list)})
    push(socket, "presence_state", presence_list)
    {:noreply, socket}
  end

  def terminate(reason, socket) do
    publish_listener_counts(socket)
    reason
  end

  def listener_count(presences) do
    presences
    |> Map.values()
    |> hd
    |> Map.get(:metas)
    |> Enum.count()
  end

  def publish_listener_counts({feed_slug, count}) do
    key = "listener_counts:" <> feed_slug

    Orcasite.Cache.put(
      key,
      count
    )

    Phoenix.PubSub.broadcast(Orcasite.PubSub, key, :update_counts)
  end

  def publish_listener_counts(socket) do
    counts = listener_count(Presence.list(socket))
    publish_listener_counts({socket.assigns.feed_slug, counts})
  end
end
