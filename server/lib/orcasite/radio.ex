defmodule Orcasite.Radio do
  use Ash.Api, extensions: [AshAdmin.Api]

  resources do
    registry Orcasite.Radio.Registry
  end

  admin do
    show? true
  end
end
