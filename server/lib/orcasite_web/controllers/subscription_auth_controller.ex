defmodule OrcasiteWeb.SubscriptionAuthController do
  use OrcasiteWeb, :controller
  use AshAuthentication.Phoenix.Controller

  def success(conn, _activity, nil = _subscription, _token) do
    return_to = get_session(conn, :return_to) || ~p"/"

    conn
    |> clear_session()
    |> redirect(to: return_to)
  end

  def success(conn, _activity, subscription, _token) when not is_nil(subscription) do
    subscription
    |> Ash.Changeset.for_update(:update, %{active: false})
    |> Orcasite.Notifications.update!()

    conn
    |> delete_session(:return_to)
    |> store_in_session(subscription)
    |> assign(:current_subscription, subscription)
    |> render("success.html")
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
