defmodule OrcasiteWeb.SubscriptionAuthHTML do
  use OrcasiteWeb, :html

  embed_templates "subscription_auth_html/*"

  def humanize_event(event_type) do
    Orcasite.Notifications.Event.humanize(event_type, true)
  end
end
