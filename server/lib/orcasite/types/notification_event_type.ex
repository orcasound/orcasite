defmodule Orcasite.Types.NotificationEventType do
  use Ash.Type.Enum, values: [:confirmed_candidate, :new_detection, :live_bout]

  def graphql_type(_), do: :notification_event_type
  def graphql_input_type(_), do: :notification_event_type
end
