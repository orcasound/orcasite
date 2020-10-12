defmodule Logfmt.Encoder do
  @moduledoc ~S"""
  Encode a `Dict` into a logfmt-style log line.

  ## Examples

      iex> Logfmt.encode [foo: "bar"]
      "foo=bar"

      iex> Logfmt.encode [foo: "bar baz", qux: true]
      "foo=\"bar baz\" qux=true"
  """

  @doc """
  See [`Logfmt.encode`](/logfmt/Logfmt.html#encode/2).
  """
  @spec encode(Dict.t, options :: Keyword.t) :: String.t
  def encode(list, options \\ []) do
    result =
      list
      |> Enum.map(&encode_pair/1)
      |> Enum.intersperse(" ")

    case options[:output] do
      # in case you wish to get back an iolist
      :iolist -> result
      # otherwise default to returning a binary
      _ -> Enum.join(result)
    end
  end

  @spec encode_value(value :: term) :: String.t
  defp encode_value(value) do
    str = Logfmt.ValueEncoder.encode(value)
    if String.match?(str, ~r/\s/) or String.contains?(str, "=") do
      ["\"", str, "\""]
    else
      str
    end
  end

  defp encode_pair({key, value}) do
    # if an options keyword list was passed in, the " " could be replaced
    # with a user configurable option, but for now we'll keep the space.
    [encode_value(key), "=", encode_value(value)]
  end
end
