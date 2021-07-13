defmodule Orcasite.MailController do
  use OrcasiteWeb, :controller
  alias Orcasite.{Email,Mailer}

  def send_mail_to_subscribers(conn, params) do
    Email.create_email("subscribers@gmail.com") |> Mailer.deliver_later()
    send_resp(conn, 200, "world")
  end
end