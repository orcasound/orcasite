defmodule OrcasiteWeb.Resolvers.Detection do
  alias Orcasite.Radio

  def submit(a, b, c) do
    IO.inspect([a,b,c])
    {:error, :not_yet_implemented}
  end
end
