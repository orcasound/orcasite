defmodule OrcasiteWeb.JsonApiRouter do
  use AshJsonApi.Router,
    domains: [Orcasite.Notifications, Orcasite.Radio],
    json_schema: "/json_schema",
    open_api: "/open_api"
end
