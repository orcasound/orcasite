defmodule Orcasite.Radio.ItemTag do
  use Ash.Resource,
    otp_app: :orcasite,
    domain: Orcasite.Radio,
    extensions: [AshAdmin.Resource, AshGraphql.Resource, AshUUID],
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer]

  resource do
    description "Tag applied by a user to an item: currently just bouts"
  end

  postgres do
    table "item_tags"
    repo Orcasite.Repo

    custom_indexes do
      index [:tag_id]
      index [:user_id]
      index [:bout_id]
    end
  end

  identities do
    identity :unique_tag, [:user_id, :tag_id, :bout_id]
  end

  attributes do
    uuid_primary_key :id

    timestamps()
  end

  calculations do
    calculate :item, :struct, Orcasite.Radio.Calculations.ItemTagItem do
      # Update contraints/output to Union type once we have other taggable items
      constraints instance_of: Orcasite.Radio.Bout
    end
  end

  relationships do
    belongs_to :user, Orcasite.Accounts.User do
      public? true
    end

    belongs_to :tag, Orcasite.Radio.Tag do
      public? true
    end

    belongs_to :bout, Orcasite.Radio.Bout do
      public? true
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

    policy action_type(:create) do
      forbid_if actor_absent()
    end

    policy action_type(:read) do
      authorize_if always()
    end

    policy action_type(:destroy) do
      authorize_if relates_to_actor_via(:user)
    end
  end

  actions do
    defaults [:read, :destroy, update: :*]

    read :for_bout do
      argument :bout_id, :string, allow_nil?: false
      filter expr(bout_id == ^arg(:bout_id))

      pagination do
        required? false
        offset? true
        countable true
      end
    end

    create :bout_tag do
      argument :tag, :map do
        allow_nil? false

        constraints fields: [
                      id: [type: :string],
                      name: [type: :string, allow_nil?: false],
                      description: [type: :string]
                    ]
      end

      argument :bout, :map do
        allow_nil? false

        constraints fields: [
                      id: [type: :string, allow_nil?: false]
                    ]
      end

      change manage_relationship(:bout, type: :append)

      change manage_relationship(:tag,
               on_lookup: :relate_and_update,
               on_no_match: :create,
               on_match: :ignore,
               on_missing: :ignore
             )

      change fn change, %{actor: current_user} ->
        change
        |> Ash.Changeset.manage_relationship(:user, current_user, type: :append)
      end
    end
  end

  graphql do
    type :item_tag

    attribute_types bout_id: :id, user_id: :id, tag_id: :id

    queries do
      list :bout_tags, :for_bout
    end

    mutations do
      create :create_bout_tag, :bout_tag
      destroy :delete_bout_tag, :destroy
    end
  end
end
