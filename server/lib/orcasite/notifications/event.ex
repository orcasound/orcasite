defmodule Orcasite.Notifications.Event do
  def types do
    [:confirmed_candidate, :new_detection]
  end

  def humanize(event_type, plural \\ false)
  def humanize(:confirmed_candidate, false), do: "confirmed candidate"
  def humanize(:confirmed_candidate, true), do: "confirmed candidates"
  def humanize(:new_detection, false), do: "new detection"
  def humanize(:new_detection, true), do: "new detections"
end
