defmodule Orcasite.Notifications.Changes.SubscriberCreate do
  use Ash.Resource.ManualCreate

  def create(changeset, _opts, _context) do
    # Get user id for this email, associate
    meta =
      case Orcasite.Accounts.User.by_email(Ash.Changeset.get_argument(changeset, :email)) do
        {:ok, %{id: user_id}} -> %{user_id: user_id}
        _ -> %{}
      end
      |> Map.put(:email, Ash.Changeset.get_argument(changeset, :email))
      |> Map.put(:response_data, Ash.Changeset.get_argument(changeset, :response_data))

    # Search for subscriber by email
    {action, upsert_changeset} =
      Ash.Changeset.get_argument(changeset, :email)
      |> Orcasite.Notifications.Subscriber.by_email()
      |> case do
        {:ok, subscriber} ->
          {:update,
           subscriber
           |> Ash.Changeset.new()
           |> Ash.Changeset.change_attribute(
             :meta,
             deep_merge(stringify_map(subscriber.meta), stringify_map(meta))
           )
           |> Ash.Changeset.for_update(:update)}

        {:error, _} ->
          {:create,
           Orcasite.Notifications.Subscriber
           |> Ash.Changeset.new()
           |> Ash.Changeset.change_attribute(:meta, meta)
           |> Ash.Changeset.change_attribute(:subscriber_type, :individual)
           |> Ash.Changeset.manage_relationship(
             :subscriptions,
             %{
               name: Ash.Changeset.get_argument(changeset, :name),
               event_type: "confirmed_candidate",
               meta: %{
                 email: Ash.Changeset.get_argument(changeset, :email),
                 name: Ash.Changeset.get_argument(changeset, :name),
                 channel: :email
               }
             },
             type: :create
           )
           |> Ash.Changeset.for_create(:create, changeset.arguments)}
      end

    apply(Orcasite.Notifications, action, [upsert_changeset])
  end

  defp deep_merge(a, b) do
    Map.merge(a, b, fn _k, v1, v2 ->
      if is_map(v1) and is_map(v2) do
        deep_merge(v1, v2)
      else
        v2
      end
    end)
  end

  defp stringify_map(map) do
    map
    |> Map.new(fn
      {k, v} when is_atom(k) ->
        {Atom.to_string(k), v}

      {k, v} ->
        {k, v}
    end)
  end
end
