defmodule OrcasiteWeb.Router do
  use OrcasiteWeb, :router

  # Set up request parsers here instead of in endpoint.ex because we don't want
  # them for the :nextjs pipeline, only :browser and :graphql
  # See https://github.com/tallarium/reverse_proxy_plug#usage-in-phoenix
  pipeline :parsers do
    plug(Plug.Parsers,
      parsers: [:urlencoded, :multipart, :json, Absinthe.Plug.Parser],
      pass: ["*/*"],
      json_decoder: Jason
    )
    plug(Plug.MethodOverride)
    plug(Plug.Head)
  end

  pipeline :browser do
    plug(:parsers)
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(:fetch_flash)
    plug(:protect_from_forgery)
    plug(:put_secure_browser_headers)
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

  scope "/" do
    if Mix.env() == :dev do
      # Use the default browser stack
      pipe_through(:browser)
      get("/*page", OrcasiteWeb.PageController, :index)
    else
      pipe_through(:nextjs)
      ui_port = System.get_env("UI_PORT") || "3000"
      forward("/", ReverseProxyPlug, upstream: "http://localhost:#{ui_port}")
    end
  end
end
