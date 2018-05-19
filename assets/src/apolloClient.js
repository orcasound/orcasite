import ApolloClient from 'apollo-boost'

import { getToken, logOut } from 'utils/auth'

export default new ApolloClient({
  request: (operation) => {
    const token = getToken() || null
    if (token) {
      operation.setContext({
        headers: {
          authorization: `Bearer ${token}`
        }
      })
    }
  },
  onError: ({ graphQLErrors, networkError }) => {
    if (networkError) {
      // logOut()
    }
  }
})
