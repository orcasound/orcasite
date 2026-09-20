defmodule Orcasite.Radio.Tag do
  use Ash.Resource,
    otp_app: :orcasite,
    domain: Orcasite.Radio,
    extensions: [AshAdmin.Resource, AshGraphql.Resource, AshJsonApi.Resource, AshSlug, AshUUID],
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer]

  resource do
    description "Tag definition with a name, description, unique slug, and optionally what kind of thing it names and the external identifier for it"
  end

  postgres do
    table "tags"
    repo Orcasite.Repo

    # The case-insensitive name index below is a custom index, so say which field a
    # violation belongs to; otherwise it is reported against `id`.
    unique_index_names [
      {[:name], "tags_lower_name_index", "is already a tag, differing only by case"}
    ]

    custom_indexes do
      index ["name gin_trgm_ops"], name: "tags_name_gin_index", using: "gin"

      # Animals, signal types and everything else share this one table, so it is the
      # only place a name collision between those vocabularies can be caught.
      index ["lower(name)"], name: "tags_lower_name_index", unique: true
    end
  end

  identities do
    identity :unique_slug, [:slug]

    # Any number of tags may have no iri (nils are distinct); no two may cite the same one.
    identity :unique_iri, [:iri]
  end

  attributes do
    uuid_primary_key :id
    attribute :name, :string, public?: true, allow_nil?: false
    attribute :description, :string, public?: true
    attribute :slug, :string, public?: true, allow_nil?: false

    attribute :kind, :atom do
      public? true
      constraints one_of: [:animal, :signal, :other]

      description """
      What the tag names: an `animal` (a species, ecotype, pod, matriline or individual),
      a `signal` (a call type such as S01), or `other` (vessels, recording quality,
      project markers). `other` is an answer, not a fallback -- it tells a consumer the
      tag is safe to skip. Nil means nobody has classified the tag yet.
      """
    end

    attribute :iri, :string do
      public? true

      description """
      The identifier this tag cites in an external catalogue, as a CURIE or a full IRI.
      An `animal` tag cites the salish-sea/animals register: `SSA:0000020` is J pod.
      Unlike the name and the slug, it survives the tag being renamed. Nil is normal:
      free-text tags stay legal, and an `animal` tag with no iri is how a gap in the
      register shows up.
      """
    end

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

  validations do
    # A prefix, a colon and a local part, with no whitespace: `SSA:0000020` or
    # `https://example.org/x`. Deliberately loose -- it is here to stop a name being
    # pasted into the identifier field, not to know every catalogue's format.
    validate match(:iri, ~r/\A[A-Za-z][A-Za-z0-9+.-]*:\S+\z/) do
      where present(:iri)
      message "must be an identifier such as SSA:0000020, not a name"
    end
  end

  actions do
    defaults [:destroy, update: :*]

    read :read do
      primary? true

      pagination do
        required? false
        offset? true
        countable true
      end
    end

    read :search do
      argument :query, :string, allow_nil?: false

      prepare build(sort: [name: :asc])
      filter expr(fragment("? ilike ?", name, expr("%" <> ^arg(:query) <> "%")))
    end

    create :create do
      primary? true
      upsert? true
      upsert_identity :unique_slug
      upsert_fields [:name, :description]

      accept [:name, :description]

      change slugify(:name, into: :slug)
    end
  end

  json_api do
    # No routes -- tags are only ever reached as an include (e.g. from bouts).
    # The type is still required so resource identifiers and included resources
    # serialize with a non-null "type".
    type "tag"
  end

  graphql do
    type :tag

    queries do
      list :tags, :read
      list :search_tags, :search
    end

    mutations do
      create :create_tag, :create
    end
  end
end
