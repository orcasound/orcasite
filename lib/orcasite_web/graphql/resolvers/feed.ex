defmodule OrcasiteWeb.Resolvers.Feed do
  def index(_, _) do
    {:ok, Orcasite.Radio.list_feeds()}
  end
end
