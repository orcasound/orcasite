defmodule Orcasite.Radio.Validations.MaxTimeDiff do
  use Ash.Resource.Validation

  @impl true
  def init(opts) do
    if is_atom(opts[:from_date_attr]) and is_atom(opts[:to_date_attr]) and
         is_integer(opts[:max_interval]) do
      {:ok, opts}
    else
      {:error,
       "Expected options `from_date_attr` and `to_date_attr` to be atoms and `max_interval` to be an integer (time interval in milliseconds), got: #{inspect(opts)}"}
    end
  end

  @impl true
  def validate(change, opts, _context) do
    from_date = change |> Ash.Changeset.get_attribute(opts[:from_date_attr])
    to_date = change |> Ash.Changeset.get_attribute(opts[:to_date_attr])
    max_interval = change |> Ash.Changeset.get_attribute(opts[:max_interval])

    if DateTime.diff(to_date, from_date, :millisecond) <= max_interval do
      :ok
    else
      {:error,
       fields: [opts[:from_date_attr], opts[:to_date_attr]],
       message: "must be no more than #{inspect(max_interval)} milliseconds apart"}
    end
  end
end
