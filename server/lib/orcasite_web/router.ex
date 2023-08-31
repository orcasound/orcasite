defmodule OrcasiteWeb.Router do
  use OrcasiteWeb, :router
  use AshAuthentication.Phoenix.Router

  import AshAdmin.Router

  require Logger

  # Set up request parsers here instead of in endpoint.ex because we don't want
  # them for the :nextjs pipeline, only :browser and :graphql
  # See https://github.com/tallarium/reverse_proxy_plug#usage-in-phoenix
  pipeline :parsers do
    plug(Plug.Parsers,
      parsers: [:urlencoded, :multipart, :json, Absinthe.Plug.Parser],
      pass: ["*/*"],
      json_decoder: Phoenix.json_library()
    )

    plug(Plug.MethodOverride)
    plug(Plug.Head)
  end

  pipeline :browser do
    plug(:parsers)
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(:fetch_live_flash)
    plug(:put_root_layout, {OrcasiteWeb.Layouts, :root})
    plug(:protect_from_forgery)
    plug(:put_secure_browser_headers)
    plug :load_from_session
  end

  pipeline :require_admin do
    plug :check_authed
    plug :check_admin_path
  end

  pipeline :nextjs do
    plug(:accepts, ["html"])
    plug(:put_secure_browser_headers)
  end

  pipeline :api do
    plug(:parsers)
    plug(:accepts, ["json"])
    plug :load_from_bearer
  end

  pipeline :graphql do
    plug(:parsers)
    plug :load_from_bearer
    plug :set_current_user_as_actor
    plug AshGraphql.Plug
  end

  scope "/graphql" do
    pipe_through(:graphql)

    forward("/", Absinthe.Plug, schema: OrcasiteWeb.Schema, json_codec: Jason)
  end

  # For the GraphiQL interactive interface, a must-have for happy frontend devs.
  scope "/graphiql" do
    pipe_through(:graphql)

    forward("/", Absinthe.Plug.GraphiQL,
      schema: OrcasiteWeb.Schema,
      interface: :playground,
      json_codec: Jason
    )
  end

  scope "/dev" do
    pipe_through(:browser)

    forward("/mailbox", Plug.Swoosh.MailboxPreview)
  end

  scope "/" do
    pipe_through [:browser, :require_admin]
    ash_admin "/admin"
  end

  scope "/s" do
    # Subscription routes
    pipe_through :browser
    sign_out_route OrcasiteWeb.SubscriberAuthController
    auth_routes_for Orcasite.Notifications.Subscriber, to: OrcasiteWeb.SubscriberAuthController

    auth_routes_for Orcasite.Notifications.Subscription,
      to: OrcasiteWeb.SubscriptionAuthController

    sign_out_route OrcasiteWeb.SubscriptionAuthController
  end

  scope "/" do
    pipe_through :browser

    sign_in_route(
      overrides: [OrcasiteWeb.AuthOverrides, AshAuthentication.Phoenix.Overrides.Default]
    )

    reset_route overrides: [
                  OrcasiteWeb.AuthOverrides,
                  AshAuthentication.Phoenix.Overrides.Default
                ]

    sign_out_route OrcasiteWeb.AuthController
    auth_routes_for Orcasite.Accounts.User, to: OrcasiteWeb.AuthController
  end

  scope "/" do
    if Mix.env() == :dev do
      pipe_through(:nextjs)
      get("/*page", OrcasiteWeb.PageController, :index)
    else
      pipe_through(:nextjs)
      ui_port = System.get_env("UI_PORT") || "3000"

      forward("/", ReverseProxyPlug,
        upstream: "http://localhost:#{ui_port}",
        error_callback: &__MODULE__.log_reverse_proxy_error/1
      )
    end
  end

  def log_reverse_proxy_error(error) do
    Logger.warn("ReverseProxyPlug network error: #{inspect(error)}")
  end

  defp check_authed(conn, _opts) do
    conn
    |> case do
      %{assigns: %{current_user: user}} when not is_nil(user) -> conn
      _ -> Phoenix.Controller.redirect(conn, to: "/sign-in")
    end
  end

  defp check_admin_path(conn, _opts) do
    # Implemented with a conditional here because AshAdmin doesn't take into account the scope for
    # paths in the dashboard
    conn
    |> case do
      %{assigns: %{current_user: %{admin: true}}, request_path: "/admin" <> _} ->
        conn

      %{request_path: "/admin" <> _} ->
        Phoenix.Controller.redirect(conn, to: "/sign-in")

      _ ->
        conn
    end
  end

  defp set_current_user_as_actor(%{assigns: %{current_user: actor}} = conn, _opts) do
    conn
    |> update_in([Access.key!(:assigns)], &Map.drop(&1, [:current_user]))
    |> Ash.PlugHelpers.set_actor(actor)
  end

  defp set_current_user_as_actor(conn, _opts), do: conn
end
