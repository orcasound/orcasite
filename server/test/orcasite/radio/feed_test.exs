defmodule Orcasite.Radio.FeedTest do
  @moduledoc """
  Regression tests for https://github.com/orcasound/orcasite/issues/974.

  The admin UI couldn't display a feed's coordinates: the show page rendered
  `<display error>` (no `Phoenix.HTML.Safe` implementation for `Geo.Point`) and
  the edit page rendered an empty "Lat Lng String" box (nothing seeded the
  argument from the record being edited).
  """

  use Orcasite.DataCase, async: true

  import Orcasite.Generators.Radio

  @point %Geo.Point{coordinates: {-123.1735774, 48.5583362}, srid: 4326, properties: %{}}

  describe "rendering a location point in a template" do
    test "renders latitude and longitude" do
      assert Phoenix.HTML.Safe.to_iodata(@point) == "48.5583362,-123.1735774"
    end
  end

  describe "the lat_lng_string argument on update" do
    setup do
      %{feed: create_feed!(location_point: @point)}
    end

    test "is seeded with the feed's current coordinates", %{feed: feed} do
      changeset = Ash.Changeset.for_update(feed, :update, %{}, authorize?: false)

      assert Ash.Changeset.get_argument(changeset, :lat_lng_string) ==
               "48.5583362,-123.1735774"
    end

    test "leaves the coordinates alone when another field is updated", %{feed: feed} do
      assert {:ok, updated} =
               feed
               |> Ash.Changeset.for_update(:update, %{name: "Somewhere Else"}, authorize?: false)
               |> Ash.update()

      assert updated.name == "Somewhere Else"
      assert updated.location_point == @point
    end

    test "still accepts new coordinates", %{feed: feed} do
      assert {:ok, updated} =
               feed
               |> Ash.Changeset.for_update(:update, %{lat_lng_string: "47.6,-122.3"},
                 authorize?: false
               )
               |> Ash.update()

      assert %Geo.Point{coordinates: {-122.3, 47.6}} = updated.location_point
    end

    test "is rendered with the current coordinates in a form", %{feed: feed} do
      form =
        feed
        |> AshPhoenix.Form.for_update(:update, domain: Orcasite.Radio, authorize?: false)
        |> Phoenix.HTML.FormData.to_form([])

      assert Phoenix.HTML.Form.input_value(form, :lat_lng_string) ==
               "48.5583362,-123.1735774"
    end
  end
end
