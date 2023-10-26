defmodule OrcasiteWeb.ListenerCountsChannel do
  use OrcasiteWeb, :channel

  def join("listener_counts:" <> feed_slug, _params, socket) do
    send(self(), :update_counts)
    {:ok, assign(socket, :feed_slug, feed_slug)}
  end

  def handle_info(:update_counts, socket) do
    listener_counts = Orcasite.Cache.get("listener_counts:" <> socket.assigns.feed_slug)

    push(socket, "listener_counts_state", %{count: listener_counts})

    {:noreply, socket}
  end
end
