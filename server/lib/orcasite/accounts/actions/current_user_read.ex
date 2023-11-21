defmodule Orcasite.Accounts.Actions.CurrentUserRead do
  use Ash.Resource.ManualRead

  @doc false
  @impl true
  def read(%{resource: resource}, _, _, %{actor: actor}) when is_struct(actor, resource),
    do: {:ok, [actor]}

  def read(_, _, _, _), do: {:ok, []}
end
