defmodule OrcasiteWeb.FeedChannel do
  use OrcasiteWeb, :channel
  alias OrcasiteWeb.Presence

  def join("feed:list", _, _) do
  end

  def join("feed:" <> feed_id, _params, socket) do
    send(self(), :after_join)
    {:ok, assign(socket, :feed_id, feed_id)}
  end

  def handle_info(:after_join, socket) do
    push(socket, "presence_state", Presence.list(socket))

    {:ok, _} =
      Presence.track(socket, socket.assigns.feed_id, %{
        online_at: inspect(System.system_time(:second))
      })

    IO.inspect(Presence.list(socket), label: "list")
    # IO.inspect(Presence.get_by_key(socket), label: "get by key")
    # require IEx
    # IEx.pry()

    {:noreply, socket}
  end

  def presence_count(presences) do
    for {key, %{metas: metas}} <- presences, into: %{} do
      {key, Enum.count(metas)}
    end
  end
end
