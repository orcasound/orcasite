defmodule Orcasite.RateLimiter do
  @moduledoc """
  Based on Boardway's Broadway.Topology.RateLimiter, but uses the Registry
  to make it distributed
  """

  use GenServer

  @atomics_index 1

  def continue?(rate_limiter, count) do
    with %{remaining: remaining} when remaining > 0 <-
           call_rate_limit(rate_limiter, count) do
      :ok
    else
      %{remaining: _remaining} = state ->
        Process.sleep(1000)
        IO.inspect(state, label: "state (server/lib/orcasite/rate_limiter.ex:#{__ENV__.line})")
        continue?(rate_limiter, count)
    end
  end

  def start_link(opts) do
    name = Keyword.fetch!(opts, :name)
    rate_limiting_opts = Keyword.fetch!(opts, :rate_limiting)
    args = {name, rate_limiting_opts}
    GenServer.start_link(__MODULE__, args, name: local_name(name))
  end

  def local_name(name) do
    Module.concat(__MODULE__, name)
  end

  def global_name(name) do
    {:via, :syn, {:rate_limiters, name}}
  end

  def syn_register(rate_limiter) do
    GenServer.cast(local_name(rate_limiter), {:register_syn, rate_limiter})
  end

  def rate_limit(counter, amount)
      when is_reference(counter) and is_integer(amount) and amount > 0 do
    :atomics.sub_get(counter, @atomics_index, amount)
  end

  def call_rate_limit(rate_limiter, amount) do
    GenServer.call(global_name(rate_limiter), {:call_rate_limit, amount})
  end

  def get_currently_allowed(counter) when is_reference(counter) do
    :atomics.get(counter, @atomics_index)
  end

  def update_rate_limiting(rate_limiter, opts) do
    GenServer.call(global_name(rate_limiter), {:update_rate_limiting, opts})
  end

  def get_rate_limiting(rate_limiter) do
    GenServer.call(global_name(rate_limiter), :get_rate_limiting)
  end

  def get_rate_limiter_ref(rate_limiter) do
    GenServer.call(global_name(rate_limiter), :get_rate_limiter_ref)
  end

  @impl GenServer
  def init({name, rate_limiting_opts}) do
    interval = Keyword.fetch!(rate_limiting_opts, :interval)
    allowed = Keyword.fetch!(rate_limiting_opts, :allowed_messages)

    counter = :atomics.new(@atomics_index, [])
    :atomics.put(counter, @atomics_index, allowed)

    _ = schedule_next_reset(interval)
    _ = schedule_syn_register()

    state = %{
      interval: interval,
      allowed: allowed,
      counter: counter,
      remaining: allowed,
      name: name
    }

    {:ok, state, {:continue, {:register_syn, name}}}
  end

  @impl GenServer
  def handle_continue({:register_syn, name}, state) do
    :syn.register(:rate_limiters, name, self())
    {:noreply, state}
  end

  @impl GenServer
  def handle_cast({:register_syn, name}, state) do
    :syn.register(:rate_limiters, name, self())
    {:noreply, state}
  end

  @impl GenServer
  def handle_call({:call_rate_limit, amount}, _from, %{counter: counter} = state) do
    remaining = rate_limit(counter, amount)
    {:reply, %{remaining: remaining}, %{state | remaining: remaining}}
  end

  @impl GenServer
  def handle_call({:update_rate_limiting, opts}, _from, state) do
    %{interval: interval, allowed: allowed} = state

    state = %{
      state
      | interval: Keyword.get(opts, :interval, interval),
        allowed: Keyword.get(opts, :allowed_messages, allowed)
    }

    {:reply, :ok, state}
  end

  def handle_call(:get_rate_limiting, _from, state) do
    %{interval: interval, allowed: allowed} = state
    {:reply, %{interval: interval, allowed_messages: allowed}, state}
  end

  def handle_call(:get_rate_limiter_ref, _from, %{counter: counter} = state) do
    {:reply, counter, state}
  end

  @impl GenServer
  def handle_info(:reset_limit, state) do
    %{interval: interval, allowed: allowed, counter: counter} = state

    :atomics.put(counter, @atomics_index, allowed)

    _ = schedule_next_reset(interval)

    {:noreply, %{state | remaining: allowed}}
  end

  @impl GenServer
  def handle_info(:syn_register, %{name: name} = state) do
    :syn.register(:rate_limiters, name, self())
    _ = schedule_syn_register()
    {:noreply, state}
  end

  defp schedule_next_reset(interval) do
    _ref = Process.send_after(self(), :reset_limit, interval)
  end

  defp schedule_syn_register() do
    _ref = Process.send_after(self(), :syn_register, 1000)
  end
end
