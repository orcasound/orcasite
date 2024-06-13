defimpl Jason.Encoder, for: Ash.ForbiddenField do
  def encode(_, _), do: "null"
  def encode!(_, _), do: "null"
end
