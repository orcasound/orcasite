defmodule OrcasiteWeb.Router do
  use OrcasiteWeb, :router
  require Logger
  use AshAuthentication.Phoenix.Router

  import AshAdmin.Router

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
    plug OrcasiteWeb.BasicAuth
  end

  pipeline :nextjs do
    plug(:accepts, ["html"])
    plug(:put_secure_browser_headers)
  end

  pipeline :authorized do
    plug(:parsers)
    plug(OrcasiteWeb.Auth.AuthAccessPipeline)
  end

  pipeline :api do
    plug(:parsers)
    plug(:accepts, ["json"])
  end

  pipeline :graphql do
    plug(:parsers)
    plug(OrcasiteWeb.Context)
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
      interface: :simple,
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

  scope "/" do
    pipe_through :browser

    sign_in_route()

    sign_out_route OrcasiteWeb.SubscriberAuthController
    auth_routes_for Orcasite.Notifications.Subscriber, to: OrcasiteWeb.SubscriberAuthController

    auth_routes_for Orcasite.Notifications.Subscription, to: OrcasiteWeb.SubscriptionAuthController
    sign_out_route OrcasiteWeb.SubscriptionAuthController

    reset_route []
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
end
