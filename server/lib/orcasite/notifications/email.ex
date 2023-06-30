defmodule Orcasite.Notifications.Email do
  import Swoosh.Email
  # import Phoenix.Component, only: [sigil_H: 2]
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
    |> subject("New detection on #{node_name}")
    |> html_body(mjml_new_detection_body(params |> Map.put(:node_name, node_name)))
  end

  def mjml_new_detection_body(assigns) do
    assigns =
      assigns
      |> Map.put(
        :unsubscribe_url,
        url(~p"/auth/subscription/unsubscribe?token=#{assigns.unsubscribe_token}")
      )

    """
    <mjml>
    <mj-body>
    <mj-section>
      <mj-column>

        <mj-text font-size="20px" font-family="helvetica">Hello World</mj-text>
        <mj-text font-size="20px" font-family="helvetica">
          A new detection has been submitted at {{ node_name }} ({{ node }})!
        </mj-text>

        <mj-text font-size="20px">
          Listen here: <a href="https://live.orcasound.net/{{node}}">https://live.orcasound.net/{{ node }}</a>
        </mj-text>

        {{#if unsubscribe_token }}
          <mj-text font-size="20px">
            If you no longer wish to receive these emails, you can unsubscribe here: {{ unsubscribe_url }}
          </mj-text>
        {{/if}}
      </mj-column>
    </mj-section>
    </mj-body>
    </mjml>
    """
    |> Zappa.compile!()
    |> EEx.eval_string(Map.to_list(assigns))
    |> Mjml.to_html()
    |> elem(1)
  end

  def new_detection_body(assigns) do
    ~H"""
    <div>
      <p>
        A new detection has been submitted at <%= @node_name %> (<%= @node %>)!
      </p>

      <p>
        Listen here:
        <a href={"https://live.orcasound.net/#{@node}"}>https://live.orcasound.net/<%= @node %></a>
      </p>

      <%= if @unsubscribe_token do %>
        <p>
          If you no longer wish to receive these emails, you can unsubscribe <a href={
            url(~p"/auth/subscription/unsubscribe?token=#{@unsubscribe_token}")
          }>here</a>.
        </p>
      <% end %>
    </div>
    """
    |> Phoenix.HTML.Safe.to_iodata()
    |> List.to_string()
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
    |> html_body(new_detection_body(params |> Map.put(:node_name, node_name)))
  end

  def confirmed_candidate_body(assigns) do
    ~H"""
    <div>
      <p>
        Don't miss the concert!
      </p>

      <p>
        Listen to orcas here:
        <a href={"https://live.orcasound.net/#{@node}"}>https://live.orcasound.net/<%= @node %></a>
      </p>

      <%= if @unsubscribe_token do %>
        <p>
          If you no longer wish to receive these emails, you can unsubscribe <a href={
            url(~p"/auth/subscription/unsubscribe?token=#{@unsubscribe_token}")
          }>here</a>.
        </p>
      <% end %>
    </div>
    """
    |> Phoenix.HTML.Safe.to_iodata()
    |> List.to_string()
  end
end
