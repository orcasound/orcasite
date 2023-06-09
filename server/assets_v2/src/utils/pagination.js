import queryString from "query-string"

export const page = ({ location, page: propsPage }) => {
  const { page } = queryString.parse(location.search)
  return Number(page || propsPage)
}

export const pageSize = ({ location, pageSize: propsPageSize }) => {
  const { pageSize } = queryString.parse(location.search)
  return Number(pageSize || propsPageSize)
}

// MUI uses 0-indexing for pages, must add offset
// First arg is props
export const onChangePage = (props, offset) => (_event, page) =>
  props.history.push({
    pathname: props.location.pathname,
    search: `page=${page + offset}&pageSize=${pageSize(props)}`
  })

export const onChangeRowsPerPage = props => ({ target: { value } }) =>
  props.history.push({
    pathname: props.location.pathname,
    search: `page=${page(props)}&pageSize=${value}`
  })
