defmodule Orcasite.Radio.MailToModerators do
    import Ecto.Query
    use Ecto.Schema
    alias Orcasite.{Email,Mailer}
    alias Orcasite.Repo
    alias Orcasite.Accounts.User


    def send_email_to_moderators do
        Repo.all(from(i in User, select: {i.email}))
        |> Enum.each(fn(x) ->
            Email.create_email(elem(x, 0)) |> Mailer.deliver_later()
           end)
    end
  end
  