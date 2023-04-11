defmodule Orcasite.Notifications.Registry do
  use Ash.Registry,
    extensions: [
      Ash.Registry.ResourceValidations
    ]

  entries do
    entry Orcasite.Notifications.Notification
  end
end
