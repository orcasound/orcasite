defmodule Orcasite.MailController do
  use OrcasiteWeb, :controller
  alias Orcasite.{Email,Mailer}

  def send_mail_to_subscribers do
    Email.create_email("subscribers@gmail.com") 
    |> Mailer.deliver_later()
  end
end