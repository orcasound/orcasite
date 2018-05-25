defmodule OrcasiteWeb.LayoutView do
  use OrcasiteWeb, :view

  def js_script_tag do
    if Mix.env() == :prod do
      ~s(<script src="/js/app.js"></script>)
    else
      ~s(<script src="http://localhost:8080/js/app.js"></script>)
    end
  end

  def css_link_tag do
    if Mix.env() == :prod do
      ~s(<link rel="stylesheet" type="text/css" href="/css/app.css" media="screen,projection" />)
    else
      ~s(<link rel="stylesheet" type="text/css" href="http://localhost:8080/css/app.css" media="screen,projection" />)
    end
  end
end
