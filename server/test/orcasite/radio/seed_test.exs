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

    test "the generic time_range action is forbidden outright" do
      assert {:error, %Ash.Error.Forbidden{}} = run_time_range()
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

    test "authorization no longer refuses the generic time_range action" do
      # Not asserting success -- that would reach the prod GraphQL API. Only
      # that the policy is no longer what stops it.
      refute match?({:error, %Ash.Error.Forbidden{}}, run_time_range())
    end
  end

  defp run_time_range do
    Seed
    |> Ash.ActionInput.for_action(:time_range, %{})
    |> Ash.run_action()
  end

  defp error_message(error), do: Exception.message(error)
end
