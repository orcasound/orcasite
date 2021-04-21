defmodule OrcasiteWeb.WebpushRouter do
  use OrcasiteWeb, :controller
  alias Orcasite.Repo

  defmodule Subscription do
    use Ecto.Schema

    schema "subscriptions" do
      field :p256dh, :string
      field :auth, :string
      field :endpoint, :string
      field :expirationTime, :string

      timestamps
    end
  end

  def index(conn, params) do
    send_resp(conn, 200, "world")
  end

  def postsubsobject(conn, params) do
    subs = params

    Repo.insert(%Subscription{ p256dh: subs["keys"]["p256dh"], 
    auth: subs["keys"]["auth"],
    endpoint: subs["endpoint"],
    expirationTime: subs["expirationTime"]})

    send_resp(conn, 200, "Subscribed successfully")
  end

  def sendnotifications(conn, params) do
    import Ecto.Query
    
    payload = ~s({"title": "#{params["title"]}", "body": "#{params["body"]}"})
    
    Repo.all(from(i in Subscription, select: {i.p256dh, i.auth, i.endpoint, i.expirationTime}))
    |> Enum.each(fn(x) ->
        IO.puts(elem(x, 1)) 
        subscription = %{
          keys: %{p256dh: elem(x, 0), auth: elem(x, 1)},
          endpoint: elem(x, 2),
          expirationTime: elem(x, 3)
        }
        WebPushEncryption.send_web_push(payload, subscription)
       end)
    send_resp(conn, 200, "Sent successfully")
  end  
end