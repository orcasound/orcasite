defmodule OrcasiteWeb.UserSocket do
  use Phoenix.Socket
  use Absinthe.Phoenix.Socket, schema: OrcasiteWeb.Schema

  ## Channels
  channel("feed:*", OrcasiteWeb.FeedChannel)
  channel("listener_counts:*", OrcasiteWeb.ListenerCountsChannel)

  # Socket params are passed from the client and can
  # be used to verify and authenticate a user. After
  # verification, you can put default assigns into
  # the socket that will be set for all channels, ie
  #
  #     {:ok, assign(socket, :user_id, verified_user_id)}
  #
  # To deny connection, return `:error`.
  #
  # See `Phoenix.Token` documentation for examples in
  # performing token verification on connect.
  @impl true
  def connect(%{"token" => token}, socket) do
    Phoenix.Token.verify(OrcasiteWeb.Endpoint, "user auth", token,
      max_age: (:timer.hours(24) / 1000) |> trunc()
    )
    |> case do
      {:ok, user_id} ->
        {:ok, user} =
          Orcasite.Accounts.User
          |> Ash.get(user_id, authorize?: false)

        {:ok,
         socket
         |> Absinthe.Phoenix.Socket.put_options(context: %{actor: user, current_user: user})}

      _ ->
        {:error, :unauthorized}
    end
  end

  def connect(_params, socket), do: {:ok, socket}

  # Socket id's are topics that allow you to identify all sockets for a given user:
  #
  #     def id(socket), do: "user_socket:#{socket.assigns.user_id}"
  #
  # Would allow you to broadcast a "disconnect" event and terminate
  # all active sockets and channels for a given user:
  #
  #     OrcasiteWeb.Endpoint.broadcast("user_socket:#{user.id}", "disconnect", %{})
  #
  # Returning `nil` makes this socket anonymous.
  @impl true
  def id(_socket), do: nil
end
