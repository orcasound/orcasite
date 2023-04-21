defmodule Orcasite.Mailer do
  use Swoosh.Mailer, otp_app: :orcasite

  def deliver_later(_), do: :ok
end
