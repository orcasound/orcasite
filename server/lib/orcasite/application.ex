defmodule Orcasite.Application do
  use Application

  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    :syn.add_node_to_scopes([:rate_limiters])

    pub_sub_redis = Application.get_env(:orcasite, :pub_sub_redis)

    pubsub_options =
      case Keyword.get(pub_sub_redis, :enabled) do
        true -> Keyword.merge(pub_sub_redis, adapter: Phoenix.PubSub.Redis)
        _ -> []
      end

    children = [
      OrcasiteWeb.Telemetry,
      Orcasite.Repo,
      Supervisor.child_spec(
        {Orcasite.RateLimiter,
         name: :ses,
         rate_limiting: [
           interval: 1_000,
           allowed_messages: 14
         ]},
        id: :ses_email_rate_limiter
      ),
      {Orcasite.Cache, []},
      {Oban, Application.fetch_env!(:orcasite, Oban)},
      {Phoenix.PubSub, Keyword.merge([name: Orcasite.PubSub], pubsub_options)},
      OrcasiteWeb.Presence,
      {Finch, name: Orcasite.Finch},
      {Task.Supervisor, name: Orcasite.TaskSupervisor},
      {AshAuthentication.Supervisor, otp_app: :orcasite},
      OrcasiteWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Orcasite.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    OrcasiteWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
