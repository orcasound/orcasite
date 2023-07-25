defmodule Orcasite.Radio.Feed do
  use Ash.Resource,
    data_layer: AshPostgres.DataLayer


  attributes do
    integer_primary_key :id

    attribute :name, :string
    attribute :node_name, :string
    attribute :slug, :string
    attribute :location_point, :geometry

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  postgres do
    table "feeds"
    repo Orcasite.Repo
  end

  identities do
    identity :unique_slug, [:slug]
  end

  actions do
    defaults [:read, :create, :update, :destroy]

    read :get_by_slug do
      get_by :slug
    end
  end

  code_interface do
    define_for Orcasite.Radio

    define :get_feed_by_slug, action: :get_by_slug, args: [:slug], get?: true
  end
end
