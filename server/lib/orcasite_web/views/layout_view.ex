defmodule OrcasiteWeb.LayoutView do
  use OrcasiteWeb, :view

  # Phoenix LiveDashboard is available only in development by default,
  # so we instruct Elixir to not warn if the dashboard route is missing.
  @compile {:no_warn_undefined, {Routes, :live_dashboard_path, 2}}

  def js_script_tag(conn) do
    if Mix.env() == :prod do
      ~s(<script src="/js/app.js"></script>)
    else
      ~s(<script src="http://#{conn.host}:8080/js/app.js"></script>)
    end
  end

  def css_link_tag(conn) do
    if Mix.env() == :prod do
      ~s(<link rel="stylesheet" type="text/css" href="/css/app.css" media="screen,projection" />)
    else
      ~s(<link rel="stylesheet" type="text/css" href="http://#{conn.host}:8080/css/app.css" media="screen,projection" />)
    end
  end
end
