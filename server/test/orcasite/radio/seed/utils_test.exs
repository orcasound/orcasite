defmodule Orcasite.Radio.Seed.UtilsTest do
  @moduledoc """
  Seeding turns production's GraphQL results into inputs for a local create
  action. Production exposes fields that the action doesn't accept -- a feed's
  `maintainerEmails`, for one -- and a single unaccepted input fails the whole
  bulk create, which left every review app without feeds.

  The results here are shaped like production's response, so nothing touches
  the network.
  """

  use Orcasite.DataCase, async: true

  alias Orcasite.Radio.Feed
  alias Orcasite.Radio.Seed.Utils

  @production_feed %{
    "name" => "Orcasound Lab",
    "nodeName" => "rpi_orcasound_lab",
    "slug" => "orcasound-lab",
    "bucket" => "audio-orcasound-net",
    "bucketRegion" => "us-west-2",
    "cloudfrontUrl" => nil,
    "introHtml" => "<p>Haro Strait</p>",
    "imageUrl" => "",
    "visible" => true,
    "locationPoint" => %{"type" => "Point", "coordinates" => [-123.17, 48.56]},
    # Readable only by admins; production returns null to anonymous callers.
    "maintainerEmails" => nil
  }

  test "drops fields the create action doesn't accept" do
    [inputs] = Utils.prepare_results([@production_feed], Feed, :create)

    refute Map.has_key?(inputs, "maintainer_emails")
    assert inputs["slug"] == "orcasound-lab"
  end

  test "the prepared inputs seed a feed" do
    inputs = Utils.prepare_results([@production_feed], Feed, :create)

    assert %{status: :success} =
             Ash.bulk_create(inputs, Feed, :create, return_errors?: true, authorize?: false)

    assert [%{slug: "orcasound-lab"}] = Ash.read!(Feed, authorize?: false)
  end
end
