defmodule OrcasiteWeb.SubscriptionOverrides do
  use AshAuthentication.Phoenix.Overrides
  alias AshAuthentication.Phoenix.Components

  override Components.Banner do
    set :image_url, "/images/logo.png"
    set :image_class, "w-80"
  end

  override Components.MagicLink.Input do
    set :submit_label, "Unsubscribe"
  end
end
