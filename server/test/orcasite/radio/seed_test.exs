defmodule Orcasite.Radio.SeedTest do
  @moduledoc """
  Tests for https://github.com/orcasound/orcasite/issues/1020.

  The seed mutations are exposed in the GraphQL schema unconditionally. A
  validation in `Orcasite.Radio.Seed` already rejects them when
  `ENABLE_SEED_FROM_PROD` is off, but resource validations only run for
  create/update/destroy actions -- not for generic actions. `seed_all` maps to
  the generic `:time_range` action, so it reached its `run` block and did work
  before failing partway through on a nested create.

  A policy now covers every action type, so seeding is refused up front.

  Nothing here invokes a seed action that would be permitted to run. Seeding
  reaches the production API over the network and writes records; these tests
  are about whether it is allowed, not about doing it.
  """

  use Orcasite.DataCase, async: false

  alias Orcasite.Radio.Seed

  setup do
    original = Application.get_env(:orcasite, :enable_seed_from_prod, false)
    on_exit(fn -> Application.put_env(:orcasite, :enable_seed_from_prod, original) end)
    :ok
  end

  defp set_seeding(enabled?), do: Application.put_env(:orcasite, :enable_seed_from_prod, enabled?)

  describe "with seeding disabled" do
    setup do
      set_seeding(false)
      :ok
    end

    test "the generic time_range action is not authorized" do
      refute Ash.can?({Seed, :time_range}, nil)
    end

    test "creating feeds is rejected" do
      assert {:error, error} = Ash.create(Seed, %{}, action: :feeds)
      assert error_message(error) =~ "Seeding is disabled"
    end

    test "seeding a resource is rejected" do
      assert {:error, error} =
               Ash.create(
                 Seed,
                 %{
                   resource: :detection,
                   feed_id: "whatever",
                   start_time: DateTime.utc_now(),
                   end_time: DateTime.utc_now()
                 },
                 action: :resource
               )

      assert error_message(error) =~ "Seeding is disabled"
    end
  end

  describe "with seeding enabled" do
    setup do
      set_seeding(true)
      :ok
    end

    test "the generic time_range action is authorized" do
      assert Ash.can?({Seed, :time_range}, nil)
    end
  end

  # Only in a build that compiled the seed actions in (ENABLE_SEED_FROM_PROD=true at
  # compile time, as review apps are built). CI builds without it, so this describe is
  # absent there; run it locally with the flag set. No network: the inputs are shaped as
  # Seed.Utils.prepare_results shapes production's GraphQL answer.
  if Application.compile_env(:orcasite, :enable_seed_from_prod, false) do
    describe "seeding a bout carries its tags" do
      alias Orcasite.Radio.{Bout, ItemTag, Tag}
      alias Orcasite.Radio.Seed.Utils

      setup do
        set_seeding(true)
        feed = Orcasite.Generators.Radio.create_feed!()
        {:ok, feed: feed}
      end

      @tag_id "2f514656-c30e-4456-8776-dd32e779e7db"

      defp prod_bout(feed, bout_id) do
        %{
          "id" => bout_id,
          "category" => "BIOPHONY",
          "startTime" => "2026-09-01T10:00:00.000000Z",
          "endTime" => "2026-09-01T10:30:00.000000Z",
          "name" => "Bigg's at the Lab",
          "duration" => "1800.0",
          "feed" => %{"id" => feed.id},
          "tags" => [
            %{
              "id" => @tag_id,
              "name" => "Bigg's",
              "description" => "Bigg's killer whale",
              "slug" => "biggs",
              "kind" => "animal",
              "iri" => "SSA:0000002"
            }
          ]
        }
      end

      defp seed!(feed, bout_id) do
        [prod_bout(feed, bout_id)]
        |> Utils.prepare_results(Bout)
        |> Ash.bulk_create!(Bout, :seed, return_errors?: true, authorize?: false)
      end

      test "creates the tag with production's id, kind and iri, joined without a user", %{
        feed: feed
      } do
        %{status: :success} = seed!(feed, "bout_0306cqy89bUJPOhwzu8zUB")

        tag = Ash.get!(Tag, @tag_id, authorize?: false)
        assert tag.name == "Bigg's"
        assert tag.kind == :animal
        assert tag.iri == "SSA:0000002"

        [join] = Ash.read!(ItemTag, authorize?: false)
        assert join.tag_id == @tag_id
        assert join.bout_id == "bout_0306cqy89bUJPOhwzu8zUB"
        assert is_nil(join.user_id)
      end

      test "seeding the same bout again adds no second tag or join row", %{feed: feed} do
        %{status: :success} = seed!(feed, "bout_031YvAeJ4O13YgkbQlc8yJ")
        %{status: :success} = seed!(feed, "bout_031YvAeJ4O13YgkbQlc8yJ")

        assert Ash.count!(Tag, authorize?: false) == 1
        assert Ash.count!(ItemTag, authorize?: false) == 1
      end

      test "a tag already here is related, not duplicated", %{feed: feed} do
        %{status: :success} = seed!(feed, "bout_030FlcX4eVsufvH9R1xbHh")
        %{status: :success} = seed!(feed, "bout_034OmhwjtcnA8JwRVVb5Av")

        assert Ash.count!(Tag, authorize?: false) == 1
        assert Ash.count!(ItemTag, authorize?: false) == 2
      end
    end
  end

  # Ash.can?/2 rather than running the action. Running :time_range executes it:
  # it seeds feeds from the production API over the network, reads them back,
  # and seeds a resource per feed. An earlier version of this test did that in
  # CI, which reached live.orcasound.net and then crashed in a spawned task
  # outside the sandboxed connection. Authorization is what these tests are
  # about, so check it directly and leave the side effects alone.

  defp error_message(error), do: Exception.message(error)
end
