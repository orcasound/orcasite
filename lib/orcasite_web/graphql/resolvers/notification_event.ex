defmodule OrcasiteWeb.Resolvers.NotificationEvent do

    alias Orcasite.Notifications
    alias Orcasite.Notifications.NotificationEvents

    def create_notification(params, _info) do
        Notifications.create_notification_event(params) 
    end

    def index(_, _) do
        {:ok, Notifications.list_notification_event()}
    end
end