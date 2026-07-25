defmodule OrcasiteWeb.JsonApiRouter.AshRouter do
  @moduledoc """
  The generated JSON:API router. Reached through `OrcasiteWeb.JsonApiRouter`,
  which runs our own plugs first.
  """

  use AshJsonApi.Router,
    domains: [Orcasite.Notifications, Orcasite.Radio],
    json_schema: "/json_schema",
    open_api: "/open_api"
end
