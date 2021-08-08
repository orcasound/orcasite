defmodule OrcasiteWeb.Router do
  use OrcasiteWeb, :router

  pipeline :browser do
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(:fetch_flash)
    plug(:protect_from_forgery)
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

  scope "/mail", Orcasite do
    pipe_through :api
    get "/subscribers", MailController, :send_mail_to_subscribers
  end

  scope "/", OrcasiteWeb do
    # Use the default browser stack
    pipe_through(:browser)
    get("/*page", PageController, :index)
  end
end
