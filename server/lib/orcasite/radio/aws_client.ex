defmodule Orcasite.Radio.AwsClient do
  alias Orcasite.Radio.{Feed, FeedStream}

  @default_timestamp_results %{count: 0, timestamps: []}

  def generate_spectrogram(%{
        image_id: image_id,
        audio_bucket: audio_bucket,
        audio_key: audio_key,
        image_bucket: image_bucket,
        image_key: image_key
      }) do
    ExAws.S3.head_object(image_bucket, String.trim_leading(image_key, "/"))
    |> ExAws.request()
    |> case do
      {:ok, %{headers: headers}} ->
        # Exists, skip
        size =
          headers
          |> Enum.find(&(elem(&1, 0) == "Content-Length"))
          |> elem(1)
          |> String.to_integer()

        {:ok, %{file_size: size}}

      _ ->
        # Doesn't exist, make spectrogram
        ExAws.Lambda.list_functions()
        |> ExAws.request!()
        |> Map.get("Functions")
        |> Enum.sort_by(&Map.get(&1, "LastModified"), :desc)
        |> Enum.find_value(fn %{"FunctionName" => name} ->
          if String.contains?(name, "AudioVizFunction"), do: {:ok, name}
        end)
        |> case do
          {:ok, name} ->
            ExAws.Lambda.invoke(
              name,
              %{
                "id" => image_id,
                "audio_bucket" => audio_bucket,
                "audio_key" => String.trim_leading(audio_key, "/"),
                "image_bucket" => image_bucket,
                "image_key" => String.trim_leading(image_key, "/")
              },
              %{},
              invocation_type: :request_response
            )
            |> ExAws.request(
              http_opts: [recv_timeout: :timer.minutes(2)],
              retries: [max_attempts: 3]
            )
            |> case do
              {:ok, %{"image_size" => image_size, "sample_rate" => sample_rate} = resp} ->
                {:ok, %{file_size: image_size, sample_rate: sample_rate, parameters: resp}}

              {:ok, %{"errorMessage" => _} = err} ->
                {:error, err}

              {:error, err} ->
                {:error, err}
            end

          _ ->
            {:error, :lambda_not_found}
        end
    end
  end

  def get_stream_manifest_body(%FeedStream{
        bucket_region: bucket_region,
        bucket: bucket,
        playlist_m3u8_path: path
      }) do
    ExAws.S3.get_object(bucket, path)
    |> ExAws.request(region: bucket_region)
    |> case do
      {:ok, %{body: body, status_code: 200}} -> {:ok, body}
      {:ok, other} -> {:error, other}
      {:error, error} -> {:error, error}
    end
  end

  def list_timestamps(%Feed{} = feed, callback \\ nil) do
    loop_request_timestamp(feed, callback)
  end

  def request_timestamps(
        %Feed{node_name: node_name, bucket: bucket, bucket_region: bucket_region},
        continuation_token \\ nil,
        callback \\ nil
      ) do
    continuation =
      case continuation_token do
        nil -> []
        "" -> []
        token -> [continuation_token: token]
      end

    options = [prefix: "#{node_name}/hls/", delimiter: "/", max_keys: 100] ++ continuation

    ExAws.S3.list_objects_v2(bucket, options)
    |> ExAws.request(region: bucket_region)
    |> case do
      {:ok, %{body: body}} ->
        timestamps =
          body
          |> Map.get(:common_prefixes)
          |> Enum.map(fn %{prefix: prefix} ->
            prefix |> String.trim_trailing("/") |> String.split("/") |> List.last()
          end)

        count = Enum.count(timestamps)

        token = body |> Map.get(:next_continuation_token)
        has_more = body |> Map.get(:is_truncated) == "true"

        if is_function(callback) do
          callback.(timestamps)
        end

        {:ok,
         %{count: count, timestamps: timestamps, continuation_token: token, has_more: has_more}}

      {:error, err} ->
        {:error, err}
    end
  end

  def loop_request_timestamp(
        feed,
        callback \\ nil,
        token \\ nil,
        results \\ @default_timestamp_results
      ) do
    request_timestamps(feed, token, callback)
    |> case do
      {:ok,
       %{
         has_more: true,
         count: count,
         timestamps: timestamps,
         continuation_token: continuation_token
       }} ->
        loop_request_timestamp(feed, callback, continuation_token, %{
          count: count + results.count,
          timestamps: results.timestamps ++ timestamps
        })

      {:ok, %{has_more: false, count: count, timestamps: timestamps}} ->
        {:ok, %{count: count + results.count, timestamps: results.timestamps ++ timestamps}}

      {:error, err} ->
        {:error, Map.put(results, :last_error, err)}
    end
  end
end
