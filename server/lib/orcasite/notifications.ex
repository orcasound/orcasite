defmodule Orcasite.Notifications do
  use Ash.Domain, extensions: [AshAdmin.Domain, AshJsonApi.Domain, AshGraphql.Domain]

  resources do
    resource Orcasite.Notifications.Notification
    resource Orcasite.Notifications.Subscriber
    resource Orcasite.Notifications.Subscription
    resource Orcasite.Notifications.NotificationInstance
    resource Orcasite.Notifications.Token
    resource Orcasite.Notifications.Job
  end

  admin do
    show? true
  end

  json_api do
    log_errors? true
  end

  graphql do
  end
end
