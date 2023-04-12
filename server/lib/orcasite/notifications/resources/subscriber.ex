defmodule Orcasite.Notifications.Subscriber do
  use Ash.Resource,
    extensions: [AshAdmin.Resource],
    data_layer: AshPostgres.DataLayer

  alias Orcasite.Notifications.{Subscription}

  resource do
    description """
    A subscriber object. Can relate to an individual, an organization, a newsletter, or an admin.
    """
  end

  actions do
    defaults [:create, :read, :update, :destroy]
  end

  postgres do
    table "subscribers"
    repo Orcasite.Repo
  end

  attributes do
    uuid_primary_key :id

    attribute :name, :string
    attribute :subscriber_type, :atom do
      constraints one_of: [:individual, :organization, :newsletter]
    end
    attribute :meta, :map

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    has_many :subscriptions, Subscription
  end
end
