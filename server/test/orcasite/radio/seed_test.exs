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

  # Ash.can?/2 rather than running the action. Running :time_range executes it:
  # it seeds feeds from the production API over the network, reads them back,
  # and seeds a resource per feed. An earlier version of this test did that in
  # CI, which reached live.orcasound.net and then crashed in a spawned task
  # outside the sandboxed connection. Authorization is what these tests are
  # about, so check it directly and leave the side effects alone.

  defp error_message(error), do: Exception.message(error)
end
