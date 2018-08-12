defmodule OrcasiteWeb.FeedChannel do
  use OrcasiteWeb, :channel
  alias OrcasiteWeb.Presence

  def join("feed:" <> feed_id, _params, socket) do
    send(self, :after_join)
    {:ok, assign(socket, :feed_id, feed_id)}
  end

  def handle_info(:after_join, socket) do
    push socket, "presence_state", Presence.list(socket)
    {:ok, _} = Presence.track(socket, socket.assigns.feed_id, %{
      online_at: inspect(System.system_time(:seconds))
    })
    {:noreply, socket}
  end
end
