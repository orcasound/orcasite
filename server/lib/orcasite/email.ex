defmodule Orcasite.Email do
    import Bamboo.Email

    def create_email(mail_sent_to) do
        new_email(
            to: mail_sent_to,
            from: "info@orcasound.net",
            subject: "SRKWs are live.",
            html_body: "<strong>Don't miss the concert!</strong>",
            text_body: "Listen Now!"
        )
    end
end