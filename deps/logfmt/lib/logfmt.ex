defmodule Logfmt do
  @moduledoc """
  Decodes and encodes logfmt-style log lines

  In [logfmt][logfmt]-style log lines, data is encoded as a string of
  `"key-value"` pairs. Logfmt can encode a `Dict` into a string in this format,
  provided that its value types are `String.t`, `number`, and `boolean`.

  It will decode a line in this format into a map, and attempt to coerce numbers
  and booleans into integers, floats, and booleans.

  [logfmt]: https://brandur.org/logfmt
  """

  @doc ~S"""
  Decodes the given line into a map

  ## Examples

      iex> Logfmt.decode "foo=bar baz=qux"
      %{"foo" => "bar", "baz" => "qux"}

      iex> Logfmt.decode ~s(foo="bar baz")
      %{"foo" => "bar baz"}

      iex> Logfmt.decode "foo=true"
      %{"foo" => true}

      iex> Logfmt.decode "foo=1"
      %{"foo" => 1}
  """
  @spec decode(String.t) :: map
  def decode(string) do
    Logfmt.Decoder.decode(string)
  end

  @doc ~S"""
  Encodes the given Dict into a Logfmt-style line

  Optionally a list of options can be given to change the encode behaviour

  Options:
  * `output` - if set to :iolist, an iolist is returned,
               any other value will return a binary

  ## Examples

      iex> Logfmt.encode [foo: "bar"]
      "foo=bar"

      iex> Logfmt.encode %{"foo" => "bar"}
      "foo=bar"

      iex> Logfmt.encode [foo: "bar baz"]
      "foo=\"bar baz\""

      iex> Logfmt.encode [foo: "bar baz"], [output: :iolist]
      [["foo", "=", ["\"", "bar baz", "\""]]]
  """
  @spec encode(Dict.t, options :: Keyword.t) :: String.t
  def encode(list, options \\ []) do
    Logfmt.Encoder.encode(list, options)
  end
end
