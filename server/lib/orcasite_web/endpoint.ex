defmodule OrcasiteWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :orcasite

  @session_options [
    store: :cookie,
    key: "_orcasite_key",
    signing_salt: "SMhQTaMpt8AcmIFwcY6BSYb6EMP+OTFx",
    same_site: "Lax"
  ]

  socket("/socket", OrcasiteWeb.UserSocket,
    websocket: [
      check_origin: if Mix.env() == :prod do
                      Map.fetch!(System.get_env(), "URLS") |> String.split(" ")
                    else
                      false
                    end
    ]
  )

  socket "/live", Phoenix.LiveView.Socket, websocket: [connect_info: [session: @session_options]]

  # Serve at "/" the static files from "priv/static" directory.
  #
  # You should set gzip to true if you are running phoenix.digest
  # when deploying your static files in production.
  plug(Plug.Static,
    at: "/",
    from: :orcasite,
    gzip: false,
    only: ~w(css fonts images js favicon.ico robots.txt)
  )

  # Code reloading can be explicitly enabled under the
  # :code_reloader configuration of your endpoint.
  if code_reloading? do
    socket("/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket)
    plug(Phoenix.LiveReloader)
    plug(Phoenix.CodeReloader)
    plug Phoenix.Ecto.CheckRepoStatus, otp_app: :orcasite
  end

  plug Phoenix.LiveDashboard.RequestLogger,
    param_key: "request_logger",
    cookie_key: "request_logger"

  plug(Plug.RequestId)
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]
  plug(Orcasite.Logger)

  # The session will be stored in the cookie and signed,
  # this means its contents can be read but not tampered with.
  # Set :encryption_salt if you would also like to encrypt it.
  # plug(Plug.Session,
  #   store: :cookie,
  #   key: "_orcasite_key",
  #   signing_salt: "uSUTZKtc"
  # )

  # Allow cross-origin requests in dev
  plug(Corsica, origins: "*", allow_headers: :all)

  plug(OrcasiteWeb.Router)

  @doc """
  Callback invoked for dynamically configuring the endpoint.

  It receives the endpoint configuration and checks if
  configuration should be loaded from the system environment.
  """
  def init(_key, config) do
    if config[:load_from_system_env] do
      port = System.get_env("PORT") || raise "expected the PORT environment variable to be set"
      {:ok, Keyword.put(config, :http, [:inet6, port: port])}
    else
      {:ok, config}
    end
  end
end
