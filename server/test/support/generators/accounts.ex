defmodule Orcasite.Generators.Accounts do
  use Ash.Generator

  def user(opts \\ []) do
    password = "password"
    {:ok, hashed_password} = AshAuthentication.BcryptProvider.hash(password)

    seed_generator(
      %Orcasite.Accounts.User{
        email: sequence(:email, &"example#{&1}@example.com"),
        first_name: StreamData.repeatedly(fn -> Faker.Person.first_name() end),
        last_name: StreamData.repeatedly(fn -> Faker.Person.last_name() end),
        username: StreamData.repeatedly(fn -> Faker.Internet.user_name() end),
        hashed_password: hashed_password
      },
      overrides: opts
    )
  end

  def create_user!(opts \\ []) do
    opts
    |> user()
    |> generate()
  end
end
