defmodule OrcasiteWeb.AudioImageChannel do
  use OrcasiteWeb, :channel

  def join("audio_image:" <> feed_id, _params, socket) do
    IO.inspect(socket, label: "joined")
    {:ok, socket |> assign(:feed_id, feed_id)}
  end

  def handle_in("updated:" <> feed_id, params, socket) do
    IO.inspect({feed_id, params}, label: "updated")
    {:noreply, socket}
  end

  def handle_in(msg, params, socket) do
    IO.inspect({msg, params}, label: "other")
    {:noreply, socket}
  end
end
