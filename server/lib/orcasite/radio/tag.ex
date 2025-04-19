defmodule Orcasite.Radio.Tag do
  use Ash.Resource,
    otp_app: :orcasite,
    domain: Orcasite.Radio,
    extensions: [AshAdmin.Resource, AshGraphql.Resource],
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer]

  resource do
    description "Tag definition with a name, description, and unique slug"
  end

  postgres do
    table "tags"
    repo Orcasite.Repo
  end

  identities do
    identity :unique_slug, [:slug]
  end

  attributes do
    uuid_primary_key :id
    attribute :name, :string, public?: true, allow_nil?: false
    attribute :description, :string, public?: true
    attribute :slug, :string, public?: true, allow_nil?: false

    timestamps()
  end

  relationships do
    has_many :item_tags, Orcasite.Radio.ItemTag

    many_to_many :bouts, Orcasite.Radio.Bout do
      through Orcasite.Radio.ItemTag
    end
  end

  policies do
    bypass actor_attribute_equals(:admin, true) do
      authorize_if always()
    end

    bypass actor_attribute_equals(:moderator, true) do
      authorize_if action_type(:create)
      authorize_if action_type(:update)
    end

    policy action_type(:read) do
      authorize_if always()
    end
  end

  actions do
    defaults [:destroy, create: :*, update: :*]

    read :read do
      primary? true

      pagination do
        required? false
        offset? true
        countable true
      end
    end
  end

  graphql do
    type :tag

    queries do
      list :tags, :read
    end

    mutations do
      create :create_tag, :create
    end
  end
end
