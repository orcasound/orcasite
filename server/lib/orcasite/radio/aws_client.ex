defmodule Orcasite.Radio.AwsClient do
  alias Orcasite.Radio.Feed

  @default_results %{count: 0, timestamps: []}

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

        token = body |> Map.get(:continuation_token)
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

  def loop_request_timestamp(feed, callback \\ nil, token \\ nil, results \\ @default_results) do
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
