defmodule Orcasite.Cache do
  use Nebulex.Cache,
    otp_app: :orcasite,
    adapter: Nebulex.Adapters.Local
end
