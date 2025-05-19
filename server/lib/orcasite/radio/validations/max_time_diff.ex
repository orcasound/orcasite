defmodule Orcasite.Radio.Validations.MaxTimeDiff do
  use Ash.Resource.Validation

  @impl true
  def init(opts) do
    if is_atom(opts[:earlier_date_attr]) and is_atom(opts[:later_date_attr]) and
         is_integer(opts[:max_time]) do
      {:ok, opts}
    else
      {:error,
       "Expected options `earlier_date_attr` and `later_date_attr` to be atoms and `max_time` to be an integer (time interval in milliseconds), got: #{inspect(opts)}"}
    end
  end

  @impl true
  def validate(change, opts, _context) do
    earlier_date = change |> Ash.Changeset.get_attribute(opts[:earlier_date_attr])
    later_date = change |> Ash.Changeset.get_attribute(opts[:later_date_attr])
    max_time = change |> Ash.Changeset.get_attribute(opts[:max_time])

    if DateTime.diff(later_date, earlier_date, :millisecond) <= max_time do
      :ok
    else
      {:error,
       fields: [opts[:earlier_date_attr], opts[:later_date_attr]],
       message: "must be no more than #{inspect(max_time)} milliseconds apart"}
    end
  end
end
