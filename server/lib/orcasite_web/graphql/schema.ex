defmodule OrcasiteWeb.Schema do
  use Absinthe.Schema

  @apis [Orcasite.Radio]

  use AshGraphql, apis: @apis

  import_types(Absinthe.Type.Custom)

  query do
  end

  mutation do
  end
end
