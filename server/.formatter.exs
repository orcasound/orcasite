[
  import_deps: [
    :ecto,
    :ecto_sql,
    :phoenix,
    :ash,
    :ash_postgres,
    :ash_admin,
    :ash_authentication,
    :ash_authentication_phoenix,
    :ash_graphql,
    :ash_json_api,
    :ash_uuid
  ],
  subdirectories: ["priv/*/migrations"],
  plugins: [Phoenix.LiveView.HTMLFormatter, Spark.Formatter],
  inputs: ["*.{heex,ex,exs}", "{config,lib,test}/**/*.{heex,ex,exs}", "priv/*/seeds.exs"],
  export: [locals_without_parens: [:plug, :uuid_attribute]]
]
