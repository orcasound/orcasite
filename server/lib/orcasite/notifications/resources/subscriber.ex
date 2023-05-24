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

    create :individual_subscriber do
      description "Create a subscriber for an individual"
      accept [:name, :email, :user_id]
      argument :name, :string
      argument :email, :string
      argument :user_id, :string

      change set_attribute(:name, arg(:name))
      change set_attribute(:subscriber_type, :individual)

      change fn changeset, _context ->
        changeset
        |> Ash.Changeset.change_attribute(:meta, %{
          user_id: Ash.Changeset.get_argument(changeset, :user_id),
          email: Ash.Changeset.get_argument(changeset, :email)
        })
      end
    end
  end

  postgres do
    table "subscribers"
    repo Orcasite.Repo
  end

  authentication do
    api Orcasite.Notifications

    tokens do
      enabled? true
      token_resource Orcasite.Notifications.SubscribeToken
      signing_secret fn _, _ ->
        Application.fetch_env(:orcasite, :token_signing_secret)
      end
    end
  end

  attributes do
    uuid_primary_key :id

    attribute :name, :string

    attribute :subscriber_type, :atom do
      constraints one_of: [:individual, :organization]
    end

    attribute :meta, :map

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    has_many :subscriptions, Subscription
  end
end
