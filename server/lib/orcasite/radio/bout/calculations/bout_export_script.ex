defmodule Orcasite.Radio.Bout.Calculations.BoutExportScript do
  use Ash.Resource.Calculation

  @impl Ash.Resource.Calculation
  def calculate(records, _opts, _context) do
    # Python script to download the bout's audio and optionally transcode locally
    # with ffmpeg
    records
    |> Enum.map(&export_script/1)
  end

  def export_script(_bout) do
    ~S|
      # A Python script to download an audio bout from live.orcasound.net. This will download ~10 second
      # .ts audio files in a bout-specific folder (orcasound_<bout_id>/) and use `ffmpeg` to combine them into a
      # single audio file in the format specified by the 'ext' option. Example:
      # `python <file_name> --ext mp4`
      import json
      import os
      import urllib.request
      import argparse
      from progress.bar import Bar


      def load_file(file_name):
          with open(file_name, "r") as f:
              bout_data = f.read()
              return json.loads(bout_data)


      def download_files(bout_json):
          bout_id = bout_json["id"]
          bout_ts_path = f"{bout_id}/ts"
          os.makedirs(bout_ts_path, exist_ok=True)  # Requires python >= 3.5
          feed_segments = bout_json["feed_segments"]
          with Bar("Downloading files", max=len(feed_segments)) as bar:
              for segment in feed_segments:
                  file_name = segment["file_name"]
                  file_path = f"{bout_ts_path}/{file_name}"
                  if not os.path.exists(file_path):
                      urllib.request.urlretrieve(segment["s3_url"], file_path)
                  bar.next()


      def combine_files(bout_json, output_ext):
          """
          Combines all ~10 second .ts audio files into one audio file
          """
          bout_id = bout_json["id"]
          bout_path = bout_id
          ts_path = "ts"
          feed_segments = sorted(
              bout_json["feed_segments"], key=lambda seg: seg["start_time"]
          )
          times = [seg["start_time"] for seg in feed_segments]
          print(f"{times =}")
          list_file = f"{bout_path}/segments.txt"

          combined_path = f"{bout_path}/{bout_id}.{output_ext}"

          write_list_file(feed_segments, ts_path, list_file)

          os.system(
              f"ffmpeg -fflags +igndts -f concat -safe 0 -i ./{list_file} -c copy ./{combined_path}"
          )


      def write_list_file(feed_segments, bout_ts_path, list_file):
          """
          Creates a list of file paths for ffmpeg to use
          https://trac.ffmpeg.org/wiki/Concatenate#Instructions
          """
          with open(list_file, "w") as f:
              for segment in feed_segments:
                  file_path = f"{bout_ts_path}/{segment["file_name"]}"
                  f.write(f"file '{file_path}'\n")


      if __name__ == "__main__":
          script_name = os.path.basename(__file__)
          bout_id = script_name.split("__")[1].split(".")[0]

          parser = argparse.ArgumentParser(
              "download_bout",
              description=f"""
              A Python script to download an audio bout from live.orcasound.net. This will download ~10 second
              .ts audio files in a bout-specific folder (orcasound_{bout_id}/) and use `ffmpeg` to combine them into a
              single audio file in the format specified by the 'ext' option. Example:

              `python {script_name} --ext mp4`
              """,
          )
          parser.add_argument(
              "ext",
              help="Extension of the combined audio file (e.g. mp4, wav, ogg, flac, etc)",
              type=str,
              default="mp4",
              const="mp4",
              nargs="?",
          )
          args = parser.parse_args()
          output = args.ext

          file_name = f"orcasound_{bout_id}.json"
          bout_json = load_file(file_name)

          download_files(bout_json)
          combine_files(bout_json, output)
    | |> String.replace("\n      ", "\n")
  end
end
