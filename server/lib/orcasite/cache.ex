defmodule Orcasite.Cache do
  use Nebulex.Cache,
    otp_app: :orcasite,
    adapter: Application.compile_env(:orcasite, :cache_adapter, Nebulex.Adapters.Local)
end
