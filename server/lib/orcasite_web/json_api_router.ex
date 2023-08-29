defmodule OrcasiteWeb.JsonApiRouter do
  use AshJsonApi.Api.Router,
    apis: [Orcasite.Notifications],
    json_schema: "/json_schema",
    open_api: "/open_api",
    modify_open_api: {__MODULE__, :modify_open_api, []}

  def modify_open_api(spec, _, _) do
    %{
      spec
      | servers: Enum.map(spec.servers, &%{&1 | url: &1.url <> "/api/json"})
    }
  end
end
