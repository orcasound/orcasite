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
    |> subject("New detection on #{node_name}")
    |> html_body(mjml_new_detection_body(params |> Map.put(:node_name, node_name)))
  end

  def mjml_new_detection_body(assigns) do
    assigns =
      assigns
      |> Map.put(
        :unsubscribe_url,
        url(~p"/s/auth/subscription/unsubscribe?token=#{assigns.unsubscribe_token}")
      )

    """
    <mjml>
    <mj-body>
    <mj-section>
      <mj-column>

        <mj-text font-size="20px" font-family="helvetica">
          A new detection has been submitted at {{ node_name }} ({{ node }})!
        </mj-text>

        <mj-text font-size="20px" font-family="helvetica">
          Description: {{#if meta["description"]}}{{ meta["description"] }}{{else}}(no description){{/if}}
        </mj-text>
        {{#if meta["listener_count"]}}
          <mj-text font-size="20px" font-family="helvetica">
            Listeners: {{ meta["listener_count"] }}
          </mj-text>
        {{/if}}

        {{#if notifications_since_count > 0}}
          <mj-text font-size="20px" font-family="helvetica">
            There have been {{ notifications_since_count }} other detections since the last notification.
          </mj-text>
          <ul>
          {{#each notifications_since as |notif_meta|}}
            <li>
              <mj-text font-size="20px" font-family="helvetica">
                {{#if notif_meta["description"]}}
                  {{ notif_meta["description"] }}
                {{else}}
                  (no description)
                {{/if}}
              </mj-text>
            </li>
          {{/each}}
          </ul>
        {{/if}}


        <mj-text font-size="20px">
          Listen here: <a href="https://live.orcasound.net/{{node}}">https://live.orcasound.net/{{ node }}</a>
        </mj-text>

        {{#if unsubscribe_token }}
          <mj-text font-size="20px">
            If you no longer wish to receive these emails, you can <a href="{{unsubscribe_url}}">unsubscribe here</a>.
          </mj-text>
        {{/if}}
      </mj-column>
    </mj-section>
    </mj-body>
    </mjml>
    """
    |> compile_mjml(assigns)
  end

  def mjml_confirmed_candidate_body(assigns) do
    assigns =
      assigns
      |> Map.put(
        :unsubscribe_url,
        url(~p"/s/auth/subscription/unsubscribe?token=#{assigns.unsubscribe_token}")
      )

    """
    <mjml>
      <mj-body>
        <!-- header -->
        <mj-section padding="0" full-width="" background-color="#c4cdd3">
          <mj-column padding="0">
            <mj-image src="https://orcasite.s3.us-west-2.amazonaws.com/email_assets/orcasound_email_header.jpg" alt="Orcasound" align="center" container-background-color="#c4cdd3"></mj-image>
          </mj-column>
        </mj-section>

        <mj-section>
          <mj-column background-color="#404040" padding="18px">
            <mj-text font-size="14px" color="#F2F2F2" font-family="Helvetica" line-height="150%">
              {{ meta.message }}
            </mj-text>
          </mj-column>
        </mj-section>

        <mj-section>
          <mj-column>
            <mj-image src="https://orcasite.s3.us-west-2.amazonaws.com/email_assets/orcasound_dont_miss.jpg"></mj-image>

            <mj-button href="http://live.orcasound.net/{{ node }}" background-color="#0F0F0F" border-radius="31px" font-size="18px" padding="18px" font-weight="bold">LISTEN NOW!</mj-button>
          </mj-column>
        </mj-section>

        <mj-section>
          <mj-column background-color="#404040">
            <mj-text color="#F2F2F2" font-size="16px" align="center" line-height="150%" font-family="Helvetica">
              If you miss the concert, <br /> watch the <a href="https://orcasound.net/blog?ecm" style="color: #007C89;">Orcasound blog</a> for recordings & bioacoustic analysis!
            </mj-text>
          </mj-column>
        </mj-section>

        <!-- footer -->
        <mj-section background-color="#5d5b72" full-width="">
          <mj-column>
            <mj-text font-weight="bold" font-family="Helvetica" color="#ffffff" align="center" line-height="150%">
              If you encounter whales at sea, <a href="https://www.bewhalewise.org/" style="font-weight: normal; color: #ffffff">Be Whale Wise</a>.<br />
              Know the laws and best practices in both the U.S. and Canada.
            </mj-text>

            <mj-divider border-color="#404040" border-width="1px"></mj-divider>

            <mj-text color="#ffffff" font-size="12px" font-style="italic" align="center" line-height="150%">
              Copyright © 2023 Orcasound, All rights reserved. <br />
              You are receiving this email because you opted in via our website.
            </mj-text>
            <mj-text color="#ffffff" font-size="12px" align="center" line-height="150%">
              <strong>Our mailing address is:</strong><br />
              Orcasound<br />
              7044 17th Ave NE<br />
              Seattle, WA 98115-5739
            </mj-text>
            {{#if unsubscribe_token }}
              <mj-text color="#ffffff" font-size="12px" align="center" line-height="150%">
                If you no longer wish to receive these emails,<br />
                you can <a href="{{ unsubscribe_url }}" style="color: #ffffff;">unsubscribe here</a>.
              </mj-text>
            {{/if}}
          </mj-column>
        </mj-section>

      </mj-body>
    </mjml>
    """
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

  def compile_mjml(mjml, assigns) do
    mjml
    |> Zappa.compile!()
    |> EEx.eval_string(Map.to_list(assigns))
    |> Mjml.to_html()
    |> elem(1)
  end
end
