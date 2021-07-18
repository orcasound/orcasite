defmodule OrcasiteWeb.Resolvers.NotificationEvent do

    alias Orcasite.Notifications
    alias Orcasite.Notifications.NotificationEvents

    def create_notification(params,  %{context: %{current_user: %{admin: true}}}) do
        Notifications.create_notification_event(params) 
    end

    def index(_,  %{context: %{current_user: %{admin: true}}}) do
        {:ok, Notifications.list_notification_event()}
    end
end