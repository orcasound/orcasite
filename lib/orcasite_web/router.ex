defmodule OrcasiteWeb.Router do
  use OrcasiteWeb, :router

  pipeline :nextjs do
    plug(:accepts, ["html"])
    plug(:put_secure_browser_headers)
  end

  pipeline :authorized do
    plug(OrcasiteWeb.Auth.AuthAccessPipeline)
  end

  pipeline :api do
    plug(:accepts, ["json"])
  end

  pipeline :graphql do
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
    pipe_through [:nextjs]
    ui_port = System.get_env("UI_PORT") || "3000"
    forward("/", ReverseProxyPlug, upstream: "http://localhost:#{ui_port}")
  end
end
