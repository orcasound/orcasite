defmodule Orcasite.Notifications.Email do
  import Swoosh.Email

  def new_detection_email(%{to: email, name: name, node: node}) do
    node_name = String.split(node, "-") |> Enum.map(&String.capitalize/1) |> Enum.join(" ")

    new()
    |> to({name, email})
    |> from({"Orcasound", "info@orcasound.net"})
    |> put_provider_option(:template_name, "new-detection-notification")
    |> put_provider_option(:global_merge_vars, [
      %{"name" => "NODE", "content" => node},
      %{"name" => "NODENAME", "content" => node_name}
    ])
  end
end
