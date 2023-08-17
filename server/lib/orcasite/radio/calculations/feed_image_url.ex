defmodule Orcasite.Radio.Calculations.FeedImageUrl do
  use Ash.Calculation

  @impl true
  def load(_query, _opts, _context) do
    [:node_name]
  end

  @impl true
  def calculate(records, opts, _arguments) do
    object = Keyword.get(opts, :object, "thumbnail.png")
    Enum.map(records, fn %{node_name: node_name} ->
      feed_s3_url(node_name, object)
    end)
  end

  def feed_s3_url(node_name, object) do
    s3_url = Application.get_env(:orcasite, :orcasite_s3_url)
    Path.join([s3_url, node_name, object])
  end
end
