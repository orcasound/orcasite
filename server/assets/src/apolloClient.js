import ApolloClient from "apollo-client"
import { InMemoryCache } from "apollo-cache-inmemory"
import { HttpLink } from "apollo-link-http"
import { onError } from "apollo-link-error"
import { setContext } from "apollo-link-context"
import { ApolloLink } from "apollo-link"

import { getToken, logOut } from "utils/auth"

export default new ApolloClient({
  cache: new InMemoryCache(),
  link: ApolloLink.from([
    setContext((_, { headers }) => {
      // get the authentication token from local storage if it exists
      const token = getToken() || null
      // return the headers to the context so httpLink can read them
      return {
        headers: {
          ...headers,
          authorization: token ? `Bearer ${token}` : ""
        }
      }
    }),
    onError(({ graphQLErrors, networkError }) => {
      if (graphQLErrors)
        graphQLErrors.forEach(({ message, locations, path }) =>
          console.log(
            `[GraphQL error]: Message: ${message}, Location: ${JSON.stringify(
              locations
            )}, Path: ${path}`
          )
        )
      if (networkError) console.log(`[Network error]: ${networkError}`)
    }),
    new HttpLink({
      uri: "/graphql",
      credentials: "same-origin"
    })
  ])
})
