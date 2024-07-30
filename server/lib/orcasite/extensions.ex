# TODO: Try removing this once upgrading ash > 3.0 and ash_json_api > 1
defimpl Jason.Encoder, for: Ash.ForbiddenField do
  def encode(_, _), do: "null"
  def encode!(_, _), do: "null"
end
