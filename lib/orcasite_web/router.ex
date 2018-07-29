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

  # Other scopes may use custom stacks.
  # scope "/api", OrcasiteWeb do
  #   pipe_through :api
  # end

  forward "/graphql", Absinthe.Plug, schema: OrcasiteWeb.Schema, json_codec: Jason
  # For the GraphiQL interactive interface, a must-have for happy frontend devs.
  forward "/graphiql", Absinthe.Plug.GraphiQL, schema: OrcasiteWeb.Schema, interface: :simple, json_codec: Jason

  scope "/", OrcasiteWeb do
    pipe_through :browser # Use the default browser stack
    get("/*page", PageController, :index)
  end

end
