defmodule OrcasiteWeb.JsonApiRouter do
  @moduledoc """
  Entry point for the JSON:API.

  Wraps `OrcasiteWeb.JsonApiRouter.AshRouter` so our own plugs can run — and
  halt — before dispatch. `AshJsonApi.Router`'s `before_dispatch` hook is not
  usable for rejecting a request: it calls the route's controller regardless of
  `conn.halted`.
  """

  use Plug.Builder

  plug OrcasiteWeb.Plugs.EnforceMaxPageSize,
    domains: [Orcasite.Notifications, Orcasite.Radio]

  plug OrcasiteWeb.JsonApiRouter.AshRouter
end
