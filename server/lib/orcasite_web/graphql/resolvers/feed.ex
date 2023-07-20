defmodule OrcasiteWeb.Resolvers.Feed do
  alias Orcasite.RadioLegacy
  def index(_, _) do
    {:ok, RadioLegacy.list_feeds()}
  end

  def show(%{slug: slug}, _) do
    {:ok, RadioLegacy.get_feed_by_slug(slug)}
  end

  def show(%{id: id}, _) do
    {:ok, RadioLegacy.get_feed!(id)}
  end
end
