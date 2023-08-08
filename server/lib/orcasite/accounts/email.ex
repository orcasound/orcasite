defmodule Orcasite.Accounts.Email do
  import Swoosh.Email
  use OrcasiteWeb, :html

  def reset_password(user, token, opts) do
    new()
    |> to({user.first_name, to_string(user.email)})
    |> from({"Orcasound", "info@orcasound.net"})
    |> subject("Reset your password")
    |> html_body(mjml_reset_password_body(%{user: user, token: token, opts: opts}))
  end

  def mjml_reset_password_body(assigns) do
    assigns =
      assigns
      |> Map.put(
        :reset_password_url,
        url(~p"/password-reset/#{assigns.token}")
      )

    """
    <mjml>
    <mj-body>
      <mj-section padding="0" full-width="" background-color="#c4cdd3">
        <mj-column padding="0">
          <mj-image src="https://orcasite.s3.us-west-2.amazonaws.com/email_assets/orcasound_email_header.jpg" alt="Orcasound" align="center" container-background-color="#c4cdd3"></mj-image>
        </mj-column>
      </mj-section>
      <mj-section>
        <mj-column>
          <mj-text line-height="150%" font-size="16px">
            Hi {{#if user.first_name}}{{user.first_name}}{{else}}there{{/if}},
          </mj-text>
          <mj-text line-height="150%" font-size="16px">
            A password reset has been requested for your Orcasound account. If you requested this, please click the link below to reset your password. Otherwise, please ignore this email.
          </mj-text>
          <mj-button href="{{reset_password_url}}" background-color="#0F0F0F" border-radius="31px" font-size="18px" padding="18px" font-weight="bold">Reset password</mj-button>
        </mj-column>
      </mj-section>

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
        </mj-column>
      </mj-section>
    </mj-body>
    </mjml>
    """
    |> compile_mjml(assigns)
  end

  def compile_mjml(mjml, assigns) do
    mjml
    |> Zappa.compile!()
    |> EEx.eval_string(Map.to_list(assigns))
    |> Mjml.to_html()
    |> elem(1)
  end
end
