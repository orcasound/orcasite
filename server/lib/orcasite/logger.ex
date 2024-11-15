defmodule Orcasite.Logger do
  @moduledoc """
  Provide standardize logfmt output on request
  """
  require Logger
  alias Plug.Conn
  @behaviour Plug

  def format(level, message, _timestamp, metadata) do
    time = DateTime.utc_now() |> DateTime.to_iso8601()
    formatted_metadata = format_metadata(metadata)

    "#{time} #{level} #{formatted_metadata}#{message}\n"
  end

  def init(opts) do
    Keyword.get(opts, :log, :info)
  end

  # Returns logfmt-formatted $message
  def call(conn, level) do
    start = System.monotonic_time()

    conn
    |> Conn.register_before_send(fn conn ->
      Logger.log level, fn ->
        stop = System.monotonic_time()
        diff = System.convert_time_unit(stop - start, :native, :microsecond)
        Logfmt.encode [
          elapsed: "#{diff / 1000}ms",
          method: conn.method,
          path: conn.request_path,
          status: conn.status,
          remote_ip: formatted_ip(conn.remote_ip)
        ]
      end

      conn
    end)
  end

  defp formatted_ip(ip) do
    to_string(:inet_parse.ntoa(ip))
  end

  defp format_metadata(meta) do
    Enum.map(meta, fn {key, val} ->
      [to_string(key), ?=, metadata(val), ?\s]
    end)
  end

  defp metadata(pid) when is_pid(pid) do
    :erlang.pid_to_list(pid)
  end

  defp metadata(ref) when is_reference(ref) do
    ~c"#Ref" ++ rest = :erlang.ref_to_list(ref)
    rest
  end

  defp metadata(atom) when is_atom(atom) do
    case Atom.to_string(atom) do
      "Elixir." <> rest -> rest
      "nil" -> ""
      binary -> binary
    end
  end

  defp metadata(other), do: to_string(other)
end
