defmodule Orcasite.Notifications.Email do
  import Swoosh.Email
  use OrcasiteWeb, :html

  def new_detection_email(
        %{
          to: email,
          name: name,
          node: node
        } = params
      ) do
    node_name = String.split(node, "-") |> Enum.map(&String.capitalize/1) |> Enum.join(" ")

    new()
    |> to({name, email})
    |> from({"Orcasound", "info@orcasound.net"})
    |> subject(
      "New detection at #{node_name}#{if params.notifications_since_count > 0, do: " [+#{params.notifications_since_count}]", else: ""}"
    )
    |> html_body(mjml_new_detection_body(params |> Map.put(:node_name, node_name)))
  end

  def mjml_new_detection_body(assigns) do
    assigns =
      assigns
      |> Map.put(
        :unsubscribe_url,
        url(~p"/s/subscription/unsubscribe?token=#{assigns.unsubscribe_token}")
      )

    read_template("new_detection")
    |> compile_mjml(assigns)
  end

  def mjml_confirmed_candidate_body(assigns) do
    assigns =
      assigns
      |> Map.put(
        :unsubscribe_url,
        url(~p"/s/subscription/unsubscribe?token=#{assigns.unsubscribe_token}")
      )

    read_template("confirmed_candidate")
    |> compile_mjml(assigns)
  end

  def confirmed_candidate_email(
        %{
          to: email,
          name: name,
          node: node
        } = params
      ) do
    node_name = String.split(node, "-") |> Enum.map(&String.capitalize/1) |> Enum.join(" ")

    new()
    |> to({name, email})
    |> from({"Orcasound", "info@orcasound.net"})
    |> subject("Listen to orcas near #{node_name}!")
    |> html_body(
      mjml_confirmed_candidate_body(
        params
        |> Map.put(:node_name, node_name)
      )
    )
  end

  defp read_template(name) do
    Path.expand("lib/orcasite/notifications/templates/#{name}.mjml.eex")
    |> File.read!()
  end

  def compile_mjml(mjml, assigns) do
    mjml
    |> Zappa.compile!()
    |> EEx.eval_string(Map.to_list(assigns))
    |> Mjml.to_html()
    |> elem(1)
  end
end
