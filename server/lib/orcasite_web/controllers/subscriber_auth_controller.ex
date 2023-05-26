defmodule OrcasiteWeb.SubscriberAuthController do
  use OrcasiteWeb, :controller
  use AshAuthentication.Phoenix.Controller

  def success(conn, _activity, subscriber, _token) do
    return_to = get_session(conn, :return_to) || ~p"/"

    IO.puts("Success!")
    IO.inspect(subscriber, label: "subscriber (server/lib/orcasite_web/controllers/auth_controller.ex:#{__ENV__.line})")

    conn
    |> delete_session(:return_to)
    |> store_in_session(subscriber)
    |> assign(:current_user, subscriber)
    |> redirect(to: return_to)
  end

  def failure(conn, _activity, _reason) do
    conn
    |> put_status(401)
    |> render("failure.html")
  end

  def sign_out(conn, _params) do
    return_to = get_session(conn, :return_to) || ~p"/"

    conn
    |> clear_session()
    |> redirect(to: return_to)
  end
end
