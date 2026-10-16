defmodule Orcasite.Radio.ItemTag do
  use Ash.Resource,
    otp_app: :orcasite,
    domain: Orcasite.Radio,
    extensions: [AshAdmin.Resource, AshGraphql.Resource, AshJsonApi.Resource, AshUUID],
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer]

  resource do
    description "Tag applied by a user to an item (currently just bouts), and how sure they were"
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

    attribute :certainty, :atom do
      public? true
      constraints one_of: [:certain, :probable, :possible]

      description """
      How sure the moderator was that this tag belongs on this bout. On the application,
      not the tag, because `L` is certain on one bout and a hedge on the next; a `?` in
      the bout's name is where that hedge went before this column existed. Three words
      rather than a number: a listening moderator has no probability, and a numeric field
      invites a UI to invent one. Nil means the moderator said nothing about it: every
      application made before the column existed, and one applied in the picker without
      touching its `?`. It is deliberately distinct from `certain`, which a moderator
      chose.
      """
    end

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

    # Before the moderators' bypass, which would otherwise let one moderator rewrite how
    # sure another was.
    policy action(:set_certainty) do
      authorize_if relates_to_actor_via(:user)
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

    # The join row a seeded bout's tags are attached through (Bout.Changes.SeedTags).
    # No user: production's moderator does not exist locally, and the column allows it.
    create :seed do
      accept [:bout_id, :tag_id]
    end

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
      accept [:certainty]

      argument :tag, :map do
        allow_nil? false

        constraints fields: [
                      id: [type: :string],
                      name: [type: :string, allow_nil?: false],
                      description: [type: :string],
                      # What the picker knows about the tag it offered (#1015): a button
                      # or a register name sends both, free text neither. ResolveTag
                      # uses them to find the tag, and to fill them in where it lacks them.
                      kind: [type: :string],
                      iri: [type: :string]
                    ]
      end

      argument :bout, :map do
        allow_nil? false

        constraints fields: [
                      id: [type: :string, allow_nil?: false]
                    ]
      end

      change manage_relationship(:bout, type: :append)

      change Orcasite.Radio.ItemTag.Changes.ResolveTag

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

      # The UI asks for these in its response (ItemTagParts). If they aren't loaded,
      # AshGraphql raises after the row is saved: the tag is applied, but the UI is told
      # the request failed.
      change load([:tag, :user])
    end

    # The `?` on a tag in the picker, which steps the moderator's own application through
    # the three words and back to saying nothing (#1015). Only how sure they are: which
    # tag, and on which bout, is removing the application and making another.
    update :set_certainty do
      accept [:certainty]
      # read back like a create (ItemTagParts)
      change load([:tag, :user])
    end
  end

  json_api do
    # No routes -- item tags are only ever reached as an include on a bout
    # (`include=item_tags.tag`), which is how a consumer sees each application's certainty
    # beside the tag it applies.
    type "item_tag"

    includes [:tag]
  end

  graphql do
    type :item_tag

    attribute_types bout_id: :id, user_id: :id, tag_id: :id

    queries do
      list :bout_tags, :for_bout
    end

    mutations do
      create :create_bout_tag, :bout_tag
      update :set_bout_tag_certainty, :set_certainty
      destroy :delete_bout_tag, :destroy
    end
  end
end
