defmodule OrcasiteWeb.Types.Pagination do
  use Absinthe.Schema.Notation

  @desc "Pagination options"
  input_object :pagination do
    field(:page, non_null(:integer))
    field(:page_size, non_null(:integer))
  end

  @desc "Pagination results via scrivener"
  object :pagination_meta do
    field(:current_page, non_null(:integer))
    field(:previous_page, :integer)
    field(:next_page, :integer)
    field(:total_entries, non_null(:integer))
    field(:total_pages, non_null(:integer))
  end
end
