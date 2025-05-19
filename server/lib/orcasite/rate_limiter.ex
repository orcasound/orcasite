defmodule Orcasite.RateLimiter do
  def continue?(keys, time, number) when is_tuple(keys),
    do: continue?(Tuple.to_list(keys), time, number)

  def continue?(keys, time, number) when is_list(keys),
    do: continue?(Enum.join(keys, ":"), time, number)

  def continue?(key, time, number) when is_atom(key),
    do: continue?(Atom.to_string(key), time, number)

  def continue?(key, time, number) do
    case Hammer.check_rate(key, time, number) do
      {:allow, _count} ->
        :ok

      {:deny, _limit} ->
        # 100ms
        Process.sleep(100)
        continue?(key, time, number)
    end
  end
end
