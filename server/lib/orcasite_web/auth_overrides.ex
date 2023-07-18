defmodule OrcasiteWeb.AuthOverrides do
  use AshAuthentication.Phoenix.Overrides
  alias AshAuthentication.Phoenix.Components

  override Components.Banner do
    set :image_url, "/images/logo.png"
    set :image_class, "w-80"
  end

  override Components.MagicLink do
    set :root_class, "hidden"
  end
end
