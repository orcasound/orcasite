defprotocol Logfmt.ValueEncoder do
  @spec encode(value :: term) :: String.t
  def encode(value)
end

defimpl Logfmt.ValueEncoder, for: BitString do
  def encode(str), do: str
end

defimpl Logfmt.ValueEncoder, for: Atom do
  def encode(atom), do: Atom.to_string(atom)
end

defimpl Logfmt.ValueEncoder, for: Integer do
  def encode(int), do: Integer.to_string(int)
end

defimpl Logfmt.ValueEncoder, for: Float do
  def encode(float), do: Float.to_string(float)
end

defimpl Logfmt.ValueEncoder, for: PID do
  def encode(pid), do: inspect(pid)
end

defimpl Logfmt.ValueEncoder, for: Reference do
  def encode(ref), do: inspect(ref)
end
