import React from 'react'
import {render} from 'react-dom'

import {ApolloProvider} from 'react-apollo'
import apolloClient from './apolloClient'

import Root from './components/Root'
import ReactGA from 'react-ga'

if (ENV.GOOGLE_ANALYTICS_ID) {
  ReactGA.initialize(ENV.GOOGLE_ANALYTICS_ID)
  ReactGA.pageview(window.location.pathname + window.location.search);
}

render(
  <ApolloProvider client={apolloClient}>
    <Root />
  </ApolloProvider>,
  document.getElementById('root'),
)
