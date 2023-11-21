defimpl AshGraphql.Error, for: AshAuthentication.Errors.AuthenticationFailed do
  def to_error(error) do
    %{
      message: error.caused_by.message,
      short_message: "invalid_credentials",
      vars: error.vars,
      code: "invalid",
      fields: []
    }
  end
end
