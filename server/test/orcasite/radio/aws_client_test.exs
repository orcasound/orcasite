defmodule Orcasite.Radio.AwsClientTest do
  use ExUnit.Case, async: true

  alias Orcasite.Radio.AwsClient

  describe "spectrogram_object_path/4" do
    test "names the file by node, day, second-resolution times, id and the requested format" do
      path =
        AwsClient.spectrogram_object_path(
          "rpi_north_sjc",
          ~U[2026-10-18 01:34:58.123456Z],
          ~U[2026-10-18 01:35:08.009876Z],
          "a705756f-d46a-4abf-96c9-d9e47df2c8ea"
        )

      assert path ==
               "/rpi_north_sjc/spectrograms/2026-10-18/2026-10-18_01-34-58Z__2026-10-18_01-35-08Z__a705756f-d46a-4abf-96c9-d9e47df2c8ea.webp"
    end

    test "the extension is the format the renderer is asked for" do
      format = AwsClient.spectrogram_parameters()["format"]
      assert format == "webp"

      path =
        AwsClient.spectrogram_object_path(
          "node",
          ~U[2026-01-01 00:00:00Z],
          ~U[2026-01-01 00:00:10Z],
          "id"
        )

      assert String.ends_with?(path, ".#{format}")
    end
  end
end
