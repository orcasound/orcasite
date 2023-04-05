defmodule OrcasiteWeb.Resolvers.Feed do
  alias Orcasite.Radio
  def index(_, _) do
    {:ok, Radio.list_feeds()}
  end

  def show(%{slug: slug}, _) do
    {:ok, Radio.get_feed_by_slug(slug)}
  end

  def show(%{id: id}, _) do
    {:ok, Radio.get_feed!(id)}
  end
end
