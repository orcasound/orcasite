defmodule OrcasiteWeb.Presence do
  @moduledoc """
  Provides presence tracking to channels and processes.

  See the [`Phoenix.Presence`](http://hexdocs.pm/phoenix/Phoenix.Presence.html)
  docs for more details.
  """
  use Phoenix.Presence,
    otp_app: :orcasite,
    pubsub_server: Orcasite.PubSub

  def init(_opts) do
    # user-land state
    {:ok, %{}}
  end

  def handle_metas(topic, %{joins: joins, leaves: leaves}, presences, state) do
    # fetch existing presence information for the joined users and broadcast the
    # event to all subscribers
    for {feed_id, _presence} <- joins do
      feed_data =
        presences
        |> Map.fetch(feed_id)
        |> case do
          {:ok, metas} -> metas
          :error -> []
        end
        |> then(&%{metas: &1})

      msg = {:join, feed_data}
      Phoenix.PubSub.broadcast(Orcasite.PubSub, topic, msg)
    end

    # fetch existing presence information for the left users and broadcast the
    # event to all subscribers
    for {feed_id, _presence} <- leaves do
      feed_data =
        presences
        |> Map.fetch(feed_id)
        |> case do
          {:ok, metas} -> metas
          :error -> []
        end
        |> then(&%{metas: &1})

      msg = {:leave, feed_data}
      Phoenix.PubSub.broadcast(Orcasite.PubSub, topic, msg)
    end

    {:ok, state}
  end
end
