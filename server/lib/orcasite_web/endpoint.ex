defmodule OrcasiteWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :orcasite
  use AshGraphql.Subscription.Endpoint

  @session_options [
    store: :cookie,
    key: "_orcasite_key",
    signing_salt: "SMhQTaMpt8AcmIFwcY6BSYb6EMP+OTFx",
    same_site: "Lax"
  ]

  if Mix.env() == :dev do
    use Plug.Debugger, otp_app: :orcasite
  end

  if sandbox = Application.compile_env(:orcasite, :sandbox) do
    plug Phoenix.Ecto.SQL.Sandbox, sandbox: sandbox
  end

  socket "/socket", OrcasiteWeb.UserSocket,
    websocket: [
      # Timeout for Heroku
      timeout: 45_000,
      connect_info: [:user_agent, session: @session_options]
    ]

  socket "/live", Phoenix.LiveView.Socket,
    websocket: [
      connect_info: [:user_agent, session: @session_options],
      # Timeout for Heroku
      timeout: 45_000
    ]

  # Serve at "/" the static files from "priv/static" directory.
  #
  # You should set gzip to true if you are running phoenix.digest
  # when deploying your static files in production.
  plug Plug.Static,
    at: "/",
    from: :orcasite,
    gzip: false,
    only: OrcasiteWeb.static_paths()

  # Code reloading can be explicitly enabled under the
  # :code_reloader configuration of your endpoint.
  if code_reloading? do
    socket("/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket)
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
    plug Phoenix.Ecto.CheckRepoStatus, otp_app: :orcasite
  end

  plug Phoenix.LiveDashboard.RequestLogger,
    param_key: "request_logger",
    cookie_key: "request_logger"

  plug Plug.RequestId
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]
  plug Orcasite.Logger

  # The session will be stored in the cookie and signed,
  # this means its contents can be read but not tampered with.
  # Set :encryption_salt if you would also like to encrypt it.
  # plug(Plug.Session,
  #   store: :cookie,
  #   key: "_orcasite_key",
  #   signing_salt: "uSUTZKtc"
  # )
  # Allow cross-origin requests in dev
  plug Corsica, origins: "*", allow_headers: :all
  plug Plug.Session, @session_options
  plug OrcasiteWeb.Router
end
