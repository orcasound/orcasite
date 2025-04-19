defmodule OrcasiteWeb.Router do
  use OrcasiteWeb, :router
  use AshAuthentication.Phoenix.Router
  import Phoenix.LiveDashboard.Router

  import AshAdmin.Router

  require Logger

  # Set up request parsers here instead of in endpoint.ex because we don't want
  # them for the :nextjs pipeline, only :browser and :graphql
  # See https://github.com/tallarium/reverse_proxy_plug#usage-in-phoenix
  pipeline :parsers do
    plug Plug.Parsers,
      parsers: [:urlencoded, :multipart, :json, Absinthe.Plug.Parser],
      pass: ["*/*"],
      json_decoder: Phoenix.json_library()

    plug Plug.MethodOverride
    plug Plug.Head
    plug RemoteIp
  end

  pipeline :browser do
    plug :parsers
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, {OrcasiteWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug :load_from_session
    plug :set_actor_ip
  end

  pipeline :require_admin do
    plug :check_admin_path
  end

  pipeline :nextjs do
    plug :accepts, ["html"]
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :parsers
    plug :accepts, ["json"]
    plug :load_from_bearer
    plug :set_actor_ip
  end

  pipeline :graphql do
    plug :parsers
    plug :fetch_session
    plug :load_from_session
    plug :set_current_user_as_actor
    plug :set_actor_ip
    plug AshGraphql.Plug
  end

  scope "/api/json" do
    pipe_through(:api)

    forward "/swaggerui",
            OpenApiSpex.Plug.SwaggerUI,
            path: "/api/json/open_api",
            title: "Orcasite JSON-API - Swagger UI",
            default_model_expand_depth: 4

    forward "/redoc",
            Redoc.Plug.RedocUI,
            spec_url: "/api/json/open_api"

    forward "/", OrcasiteWeb.JsonApiRouter
  end

  scope "/graphql" do
    pipe_through(:graphql)

    forward("/", Absinthe.Plug,
      schema: Module.concat(["OrcasiteWeb.Schema"]),
      json_codec: Jason,
      before_send: {__MODULE__, :absinthe_before_send}
    )
  end

  # For the GraphiQL interactive interface, a must-have for happy frontend devs.
  scope "/graphiql" do
    pipe_through(:graphql)

    forward("/", Absinthe.Plug.GraphiQL,
      schema: Module.concat(["OrcasiteWeb.Schema"]),
      interface: :playground,
      json_codec: Jason,
      before_send: {__MODULE__, :absinthe_before_send},
      socket: OrcasiteWeb.UserSocket
    )
  end

  scope "/dev" do
    pipe_through(:browser)

    forward("/mailbox", Plug.Swoosh.MailboxPreview)
  end

  scope "/" do
    pipe_through :browser

    reset_route path: "/admin/password-reset",
                overrides: [
                  OrcasiteWeb.AuthOverrides,
                  AshAuthentication.Phoenix.Overrides.Default
                ]

    auth_routes_for Orcasite.Accounts.User, to: OrcasiteWeb.AuthController, path: "/admin"
  end

  scope "/" do
    pipe_through [:browser, :require_admin]

    live_dashboard "/admin/dashboard",
      metrics: OrcasiteWeb.Telemetry,
      ecto_repos: [Orcasite.Repo],
      ecto_psql_extras_options: [long_running_queries: [threshold: "200 milliseconds"]]

    sign_in_route(
      path: "/admin/sign-in",
      overrides: [OrcasiteWeb.AuthOverrides, AshAuthentication.Phoenix.Overrides.Default]
    )

    sign_out_route OrcasiteWeb.AuthController, "/admin/sign-out"
    ash_admin "/admin"
  end

  scope "/s" do
    # Subscription routes
    pipe_through :browser
    sign_out_route OrcasiteWeb.SubscriberAuthController
    auth_routes_for Orcasite.Notifications.Subscriber, to: OrcasiteWeb.SubscriberAuthController

    auth_routes_for Orcasite.Notifications.Subscription,
      to: OrcasiteWeb.SubscriptionAuthController
  end

  scope "/" do
    pipe_through(:nextjs)
    ui_port = System.get_env("UI_PORT") || "3000"

    forward("/", ReverseProxyPlug,
      upstream: "http://localhost:#{ui_port}",
      error_callback: &__MODULE__.log_reverse_proxy_error/1
    )
  end

  def log_reverse_proxy_error(error) do
    Logger.warning("ReverseProxyPlug network error: #{inspect(error)}")
  end

  defp check_admin_path(conn, _opts) do
    # Implemented with a conditional here because AshAdmin doesn't take into account the scope for
    # paths in the dashboard
    conn
    |> case do
      %{assigns: %{current_user: %{admin: true}}, request_path: "/admin" <> _} ->
        conn

      %{request_path: "/admin/sign-in"} ->
        conn

      %{request_path: "/admin" <> _} ->
        Phoenix.Controller.redirect(conn, to: "/admin/sign-in")

      _ ->
        conn
    end
  end

  def absinthe_before_send(conn, %Absinthe.Blueprint{} = blueprint) do
    blueprint.execution.context
    |> case do
      %{current_user: user} when not is_nil(user) ->
        conn
        |> assign(:current_user, user)
        |> AshAuthentication.Plug.Helpers.store_in_session(user)
        |> AshAuthentication.Plug.Helpers.set_actor(:user)

      %{clear_session: true} ->
        conn
        |> assign(:current_user, nil)
        |> AshAuthentication.Plug.Helpers.set_actor(:user)
        |> clear_session()

      _ ->
        conn
    end
  end

  def absinthe_before_send(conn, _) do
    conn
  end

  defp set_current_user_as_actor(%{assigns: %{current_user: _actor}} = conn, _opts) do
    conn
    |> AshAuthentication.Plug.Helpers.set_actor(:user)
  end

  defp set_current_user_as_actor(conn, _opts), do: conn

  def set_actor_ip(conn, _opts) do
    actor_ip =
      conn.remote_ip
      |> Tuple.to_list()
      |> Enum.join(".")

    Ash.PlugHelpers.set_context(conn, %{actor_ip: actor_ip})
  end
end
