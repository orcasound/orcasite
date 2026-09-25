defmodule OrcasiteWeb.SignOutTest do
  use OrcasiteWeb.ConnCase

  import OrcasiteWeb.TestSupport.AuthenticationHelper, only: [create_user: 2]

  @admin_params %{
    email: "admin@example.com",
    username: "admin",
    password: "password",
    password_confirmation: "password"
  }

  setup %{conn: conn} do
    create_user(@admin_params, admin: true)

    conn =
      post(conn, "/admin/user/password/sign_in", %{
        "user" => %{"email" => @admin_params.email, "password" => @admin_params.password}
      })

    assert get(recycle(conn), "/admin").status == 200

    [conn: recycle(conn)]
  end

  test "GET /admin/sign-out asks for confirmation instead of signing out", %{conn: conn} do
    assert conn |> get("/admin/sign-out") |> html_response(200) =~ "phx-"
    assert get(conn, "/admin").status == 200
  end

  # The confirmation page submits `<.form method="delete">`, i.e. a POST with
  # `_method=delete`. The router can't route it as a DELETE because
  # Plug.MethodOverride runs in a pipeline, after the route has been matched.
  test "submitting the confirmation form signs the admin out", %{conn: conn} do
    conn = post(conn, "/admin/sign-out", %{"_method" => "delete"})
    assert redirected_to(conn) == "/"

    assert conn |> recycle() |> get("/admin") |> redirected_to() == "/admin/sign-in"
  end

  for path <- ["/sign-out", "/s/subscriber/sign-out", "/s/subscription/sign-out"] do
    test "POST #{path} is routed", %{conn: conn} do
      assert conn |> post(unquote(path), %{"_method" => "delete"}) |> redirected_to()
    end
  end
end
