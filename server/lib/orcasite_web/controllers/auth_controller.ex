defmodule OrcasiteWeb.AuthController do
  use OrcasiteWeb, :controller
  use AshAuthentication.Phoenix.Controller

  def success(conn, _activity, nil = _user, _token) do
    return_to = get_session(conn, :return_to) || ~p"/"

    conn
    |> clear_session(:orcasite)
    |> redirect(to: return_to)
  end

  def success(conn, _activity, user, _token) do
    return_to =
      case user do
        %{admin: true} ->
          ~p"/admin"

        _ ->
          get_session(conn, :return_to) || ~p"/"
      end

    conn
    |> delete_session(:return_to)
    |> store_in_session(user)
    |> assign(:current_user, user)
    |> redirect(to: return_to)
  end

  def failure(conn, _activity, _reason) do
    conn
    |> put_flash(:error, "Couldn't sign in: email not found or password was wrong.")
    |> redirect(to: ~p"/sign-in")
  end

  def sign_out(conn, _params) do
    return_to = get_session(conn, :return_to) || ~p"/"

    conn
    |> clear_session(:orcasite)
    |> redirect(to: return_to)
  end
end
