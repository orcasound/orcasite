defmodule Orcasite.MailTest do
  use Orcasite.DataCase
  alias Orcasite.Radio.MailToModerators
  alias Orcasite.MailController

  test "send mail to moderators" do
    expected_email = MailToModerators.send_email_to_moderators
    assert expected_email == :ok
  end

  test "send_mail_to_subscribers" do
    expected_email = MailController.send_mail_to_subscribers
    expected_email == {:ok, %Bamboo.Email{}}
  end
end