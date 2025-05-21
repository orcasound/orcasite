defmodule Orcasite.Application do
  use Application

  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    pub_sub_redis = Application.get_env(:orcasite, :pub_sub_redis, [])

    pubsub_options =
      case(Keyword.get(pub_sub_redis, :enabled)) do
        true -> Keyword.merge(pub_sub_redis, adapter: Phoenix.PubSub.Redis)
        _ -> []
      end

    children =
      [
        OrcasiteWeb.Telemetry,
        Orcasite.Repo,
        {Orcasite.Cache, []},
        {Oban, Application.fetch_env!(:orcasite, Oban)},
        {Phoenix.PubSub, Keyword.merge([name: Orcasite.PubSub], pubsub_options)},
        OrcasiteWeb.Presence,
        {Finch, name: Orcasite.Finch},
        {Task.Supervisor, name: Orcasite.TaskSupervisor},
        {AshAuthentication.Supervisor, otp_app: :orcasite},
        if(Application.get_env(:orcasite, :feed_stream_queue_url) not in [nil, ""],
          do: {Orcasite.Radio.FeedStreamQueue, []}
        ),
        OrcasiteWeb.Endpoint,
        {Absinthe.Subscription, OrcasiteWeb.Endpoint}
      ]
      |> Enum.filter(&Function.identity/1)

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
