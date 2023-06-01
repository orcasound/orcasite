defmodule Orcasite.Notifications.Registry do
  use Ash.Registry,
    extensions: [
      Ash.Registry.ResourceValidations
    ]

  entries do
    entry Orcasite.Notifications.Notification
    entry Orcasite.Notifications.Subscriber
    entry Orcasite.Notifications.Subscription
    entry Orcasite.Notifications.SubscriptionNotification
    entry Orcasite.Notifications.Token
  end
end
