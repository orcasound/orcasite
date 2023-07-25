defmodule OrcasiteWeb.Resolvers.Feed do
  def index(_, _) do
    {:ok, Orcasite.Radio.read!(Orcasite.Radio.Feed)}
  end

  def show(%{slug: slug}, _) do
    {:ok, Orcasite.Radio.Feed.get_feed_by_slug!(slug)}
  end

  def show(%{id: id}, _) do
    {:ok, Orcasite.Radio.get!(Orcasite.Radio.Feed, id)}
  end
end
