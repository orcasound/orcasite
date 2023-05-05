defmodule Orcasite.Notifications.Newsletter do
  def confirmed_candidate_campaign(%{node: node, subject: subject, title: title}) do
    node_name = String.split(node, "-") |> Enum.map(&String.capitalize/1) |> Enum.join(" ")
    message = "A new detection has been made on the #{node_name} hydrophone node."
    %{node: node}
  end

  def new_detection_campaign(%{node: node, subject: subject, title: title}) do
    # New detection template

    node_name = String.split(node, "-") |> Enum.map(&String.capitalize/1) |> Enum.join(" ")
    message = "A new detection has been made on the #{node_name} hydrophone node."
    %{node: node}
  end


  def main_list_id, do: "abbc7333b4" # Main list
  def confirmed_candidate_template_id, do: 10_090_581
  def new_detection_template_id, do: 10_090_585
end
