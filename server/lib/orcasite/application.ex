defmodule Orcasite.Application do
  use Application

  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    children = [
      OrcasiteWeb.Telemetry,
      Orcasite.Repo,
      {Oban, Application.fetch_env!(:orcasite, Oban)},
      {Phoenix.PubSub, name: Orcasite.PubSub},
      {Finch, name: Orcasite.Finch},
      {Task.Supervisor, name: Orcasite.TaskSupervisor},
      OrcasiteWeb.Endpoint,
      OrcasiteWeb.Presence
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
