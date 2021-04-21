defmodule OrcasiteWeb.Router do
  use OrcasiteWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :graphql do
    plug OrcasiteWeb.Context
  end

  scope "/graphql" do
    pipe_through :graphql

    forward "/", Absinthe.Plug, schema: OrcasiteWeb.Schema, json_codec: Jason
  end

  # For the GraphiQL interactive interface, a must-have for happy frontend devs.
  scope "/graphiql" do
    pipe_through :graphql
    forward "/", Absinthe.Plug.GraphiQL, schema: OrcasiteWeb.Schema, interface: :simple, json_codec: Jason
  end

  scope "/webpush", OrcasiteWeb do
    pipe_through :api
    get "/hello", WebpushRouter, :index
    post "/notifications/subscribe", WebpushRouter, :postsubsobject
    get "/notifications/payload", WebpushRouter, :sendnotifications
  end

  scope "/", OrcasiteWeb do
    pipe_through :browser # Use the default browser stack
    get("/*page", PageController, :index)
  end

end
