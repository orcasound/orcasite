defmodule Orcasite.Validations.Compare do
  use Ash.Resource.Validation

  def validate(changeset, [{op, [arg_key_1, arg_key_2]}], _context) do
    arg_1 = changeset |> Ash.Changeset.get_argument(arg_key_1)
    arg_2 = changeset |> Ash.Changeset.get_argument(arg_key_2)

    case {compare(arg_1, arg_2), op} do
      {:eq, :gte} -> :ok
      {:gt, :gte} -> :ok
      {:lt, :lte} -> :ok
      {:lt, :lt} -> :ok
      {:eq, :eq} -> :ok
      _ -> {:error, "#{arg_key_1} (#{arg_1}) is not #{op} #{arg_key_2} (#{arg_2})"}
    end
  end

  def compare(%DateTime{} = datetime_1, %DateTime{} = datetime_2) do
    DateTime.compare(datetime_1, datetime_2)
  end
end
