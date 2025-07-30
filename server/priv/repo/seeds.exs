require Ash.Query

Orcasite.Radio.Seed.time_range(%{
  end_time: DateTime.utc_now(),
  start_time: DateTime.add(DateTime.utc_now(), -1, :hour)
})

Orcasite.Radio.Seed.latest()

# Create admin account
strategy = AshAuthentication.Info.strategy!(Orcasite.Accounts.User, :password)

Orcasite.Accounts.User
|> Ash.Changeset.for_create(strategy.register_action_name, %{
  email: "admin@example.com",
  password: "password",
  password_confirmation: "password"
})
|> Ash.Changeset.force_change_attribute(:admin, true)
|> Ash.Changeset.force_change_attribute(:moderator, true)
|> Ash.create(authorize?: false)
