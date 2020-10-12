defmodule Logfmt.Mixfile do
  use Mix.Project

  @version "3.3.0"

  def project do
    [app: :logfmt,
     version: @version,
     elixir: "~> 1.3",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     source_url: "https://github.com/jclem/logfmt-elixir",
     deps: deps,

     # Hex
     description: description,
     package: package
     ]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [applications: [:logger]]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type `mix help deps` for more examples and options
  defp deps do
    [{:earmark, "~> 1.0.3", only: :dev},
     {:ex_doc, "~> 0.14.5", only: :dev}]
  end

  defp description do
    """
    Logfmt is a module for encoding and decoding logfmt-style log lines.
    """
  end

  defp package do
    [contributors: ["Jonathan Clem <jotclem@gmail.com>"],
    licenses: ["MIT"],
    links: %{"GitHub" => "https://github.com/jclem/logfmt-elixir"},
    maintainers: ["Jonathan Clem <jonathan@jclem.net>"],
    files: ~w(mix.exs lib README.md)]
  end
end
