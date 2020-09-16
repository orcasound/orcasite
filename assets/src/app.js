import React from 'react'
import {render} from 'react-dom'

import {ApolloProvider} from 'react-apollo'
import apolloClient from './apolloClient'

import Root from './components/Root'
import ReactGA from 'react-ga'

if (ENV.GOOGLE_ANALYTICS_ID) {
  ReactGA.initialize(ENV.GOOGLE_ANALYTICS_ID)
  ReactGA.modalview("/");
  //console.log(window.localtion.pathname + window.location.search);
  ReactGA.modalview('/bush-point');
  ReactGA.modalview('/port-townsend');
  ReactGA.modalview('/orcasound-lab');
  ReactGA.modalview('/paul-test');
  ReactGA.modalview('/steve-test');


}

render(
  <ApolloProvider client={apolloClient}>
    <Root />
  </ApolloProvider>,
  document.getElementById('root'),
)
